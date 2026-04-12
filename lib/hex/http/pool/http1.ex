defmodule Hex.HTTP.Pool.HTTP1 do
  @moduledoc false

  # HTTP/1 connection pool manager.
  #
  # One GenServer per {scheme, host, port}. Owns a fixed-size pool of
  # Hex.HTTP.Pool.HTTP1.Worker processes, each of which holds one Mint
  # HTTP/1 connection. Callers check out a worker (blocking if the pool is
  # full), send their request, then the worker is returned to the idle set
  # when the request completes.

  use GenServer

  alias Hex.HTTP.Pool.HTTP1.Worker

  @default_size 8

  def start_link({key, connect_fun, opts}) do
    case Keyword.fetch(opts, :name) do
      {:ok, name} -> GenServer.start_link(__MODULE__, {key, connect_fun, opts}, name: name)
      :error -> GenServer.start_link(__MODULE__, {key, connect_fun, opts})
    end
  end

  @doc """
  Performs a request on the pool identified by `pid`.

  Options:
    * `:timeout` - overall request timeout (default 15_000)
    * `:initial_conn` - an already-connected Mint conn to seed the first worker
  """
  def request(pid, method, path, headers, body, opts) do
    timeout = Keyword.get(opts, :timeout, 15_000)

    case GenServer.call(pid, {:checkout, opts[:initial_conn]}, timeout) do
      {:ok, worker} ->
        try do
          Worker.request(worker, method, path, headers, body, timeout)
        after
          GenServer.cast(pid, {:checkin, worker})
        end

      {:error, reason} ->
        {:error, reason}
    end
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
  end

  @impl true
  def init({key, connect_fun, opts}) do
    size = Keyword.get(opts, :size, @default_size)

    state = %{
      key: key,
      connect_fun: connect_fun,
      size: size,
      idle: [],
      busy: MapSet.new(),
      waiters: :queue.new(),
      monitors: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:checkout, initial_conn}, from, state) do
    cond do
      state.idle != [] ->
        [worker | rest] = state.idle
        state = %{state | idle: rest, busy: MapSet.put(state.busy, worker)}
        {:reply, {:ok, worker}, state}

      map_size(state.monitors) < state.size ->
        case start_worker(state, initial_conn) do
          {:ok, worker, state} ->
            state = %{state | busy: MapSet.put(state.busy, worker)}
            {:reply, {:ok, worker}, state}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      true ->
        # Pool full: enqueue waiter
        state = %{state | waiters: :queue.in(from, state.waiters)}
        {:noreply, state}
    end
  end

  @impl true
  def handle_cast({:checkin, worker}, state) do
    if MapSet.member?(state.busy, worker) and Process.alive?(worker) do
      state = %{state | busy: MapSet.delete(state.busy, worker)}

      case :queue.out(state.waiters) do
        {{:value, from}, waiters} ->
          state = %{state | waiters: waiters, busy: MapSet.put(state.busy, worker)}
          GenServer.reply(from, {:ok, worker})
          {:noreply, state}

        {:empty, _} ->
          {:noreply, %{state | idle: [worker | state.idle]}}
      end
    else
      state = %{state | busy: MapSet.delete(state.busy, worker)}
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, pid, _reason}, state) do
    case Map.pop(state.monitors, ref) do
      {nil, _} ->
        {:noreply, state}

      {^pid, monitors} ->
        state = %{
          state
          | monitors: monitors,
            idle: List.delete(state.idle, pid),
            busy: MapSet.delete(state.busy, pid)
        }

        # If any waiters, try to spawn a replacement
        case :queue.out(state.waiters) do
          {{:value, from}, waiters} ->
            state = %{state | waiters: waiters}

            case start_worker(state, nil) do
              {:ok, worker, state} ->
                state = %{state | busy: MapSet.put(state.busy, worker)}
                GenServer.reply(from, {:ok, worker})
                {:noreply, state}

              {:error, reason} ->
                GenServer.reply(from, {:error, reason})
                {:noreply, state}
            end

          {:empty, _} ->
            {:noreply, state}
        end
    end
  end

  def handle_info(_, state), do: {:noreply, state}

  ## Helpers

  defp start_worker(state, initial_conn) do
    result =
      if initial_conn do
        Worker.start_link({state.key, state.connect_fun, initial_conn})
      else
        Worker.start_link({state.key, state.connect_fun})
      end

    case result do
      {:ok, pid} ->
        ref = Process.monitor(pid)
        state = %{state | monitors: Map.put(state.monitors, ref, pid)}
        {:ok, pid, state}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
