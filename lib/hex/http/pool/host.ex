defmodule Hex.HTTP.Pool.Host do
  @moduledoc false

  # Per-host pool. One GenServer per {scheme, host, port}.
  #
  # On start we spawn two probe `Conn` processes. When the first probe reports
  # back with the negotiated protocol we decide the target pool size:
  #
  #   * `:http2` → stay at 2 conns (HTTP/2 multiplexes, and a second conn gives
  #     us redundancy while one is draining under GOAWAY).
  #   * `:http1` → scale up to 8 conns to match the previous `:httpc`
  #     `max_sessions` behaviour (one in-flight request per conn).
  #
  # Dispatch picks the conn with the fewest in-flight requests that still has
  # free capacity (capacity is 1 for HTTP/1 and the server's advertised
  # `max_concurrent_streams` for HTTP/2). When no conn has capacity, callers
  # are enqueued and drained as requests finish.
  #
  # Requests are forwarded to the chosen `Conn` via cast with the caller's
  # `from` tuple; the Conn replies directly to the caller and casts `:req_done`
  # back here so we can decrement its in-flight count.
  #
  # Conns stay alive for the life of the BEAM. Hex runs as a CLI that exits
  # at the end of the Mix task, at which point the supervisor terminates the
  # pool and each Conn closes its socket in terminate/2 — so an idle-timeout
  # reap path would only add complexity without saving real resources.

  use GenServer

  alias Hex.HTTP.Pool.Conn

  @initial_size 2
  @http1_size 8

  def start_link({key, connect_opts, opts}) do
    case Keyword.fetch(opts, :name) do
      {:ok, name} -> GenServer.start_link(__MODULE__, {key, connect_opts}, name: name)
      :error -> GenServer.start_link(__MODULE__, {key, connect_opts})
    end
  end

  def request(pid, method, path, headers, body, timeout) do
    GenServer.call(pid, {:request, method, path, headers, body}, timeout)
  catch
    :exit, {:timeout, _} -> {:error, :timeout}
    :exit, {reason, _} -> {:error, reason}
  end

  @impl true
  def init({key, connect_opts}) do
    Process.flag(:trap_exit, true)

    state = %{
      key: key,
      connect_opts: connect_opts,
      protocol: nil,
      target_size: @initial_size,
      conns: %{},
      waiters: :queue.new()
    }

    state = Enum.reduce(1..@initial_size, state, fn _, s -> start_conn(s) end)
    {:ok, state}
  end

  @impl true
  def handle_call({:request, method, path, headers, body}, from, state) do
    case pick_conn(state) do
      {:ok, conn_pid, state} ->
        GenServer.cast(conn_pid, {:request, from, method, path, headers, body})
        {:noreply, state}

      :no_capacity ->
        waiters = :queue.in({from, method, path, headers, body}, state.waiters)
        {:noreply, %{state | waiters: waiters}}
    end
  end

  @impl true
  def handle_cast({:conn_ready, conn_pid, protocol, capacity}, state) do
    state =
      update_conn(state, conn_pid, fn info ->
        %{info | ready: true, capacity: capacity, in_flight: 0}
      end)

    state = maybe_set_protocol(state, protocol)
    state = drain_waiters(state)
    {:noreply, state}
  end

  def handle_cast({:conn_draining, conn_pid}, state) do
    state = update_conn(state, conn_pid, fn info -> %{info | ready: false} end)
    {:noreply, state}
  end

  def handle_cast({:conn_down, conn_pid, _reason}, state) do
    state =
      update_conn(state, conn_pid, fn info ->
        %{info | ready: false, in_flight: 0}
      end)

    {:noreply, state}
  end

  def handle_cast({:req_done, conn_pid}, state) do
    state =
      update_conn(state, conn_pid, fn info ->
        %{info | in_flight: max(info.in_flight - 1, 0)}
      end)

    {:noreply, drain_waiters(state)}
  end

  @impl true
  def handle_info({:EXIT, pid, _reason}, state) do
    case Map.pop(state.conns, pid) do
      {nil, _} ->
        {:noreply, state}

      {_info, conns} ->
        state = %{state | conns: conns}
        {:noreply, start_conn(state)}
    end
  end

  def handle_info(_, state), do: {:noreply, state}

  ## Conn lifecycle

  defp start_conn(state) do
    case Conn.start_link({self(), state.key, state.connect_opts}) do
      {:ok, pid} ->
        info = %{ready: false, in_flight: 0, capacity: 0}
        %{state | conns: Map.put(state.conns, pid, info)}

      {:error, _reason} ->
        state
    end
  end

  defp update_conn(state, pid, fun) do
    case Map.get(state.conns, pid) do
      nil -> state
      info -> %{state | conns: Map.put(state.conns, pid, fun.(info))}
    end
  end

  defp maybe_set_protocol(%{protocol: nil} = state, :http1) do
    state = %{state | protocol: :http1, target_size: @http1_size}
    needed = @http1_size - map_size(state.conns)

    if needed > 0 do
      Enum.reduce(1..needed, state, fn _, s -> start_conn(s) end)
    else
      state
    end
  end

  defp maybe_set_protocol(%{protocol: nil} = state, :http2),
    do: %{state | protocol: :http2}

  defp maybe_set_protocol(state, _), do: state

  ## Dispatch

  defp pick_conn(state) do
    best =
      state.conns
      |> Enum.filter(fn {_pid, info} ->
        info.ready and info.in_flight < info.capacity
      end)
      |> Enum.min_by(fn {_pid, info} -> info.in_flight end, fn -> nil end)

    case best do
      nil ->
        :no_capacity

      {pid, info} ->
        conns = Map.put(state.conns, pid, %{info | in_flight: info.in_flight + 1})
        {:ok, pid, %{state | conns: conns}}
    end
  end

  defp drain_waiters(state) do
    if :queue.is_empty(state.waiters) do
      state
    else
      case pick_conn(state) do
        {:ok, conn_pid, state} ->
          {{:value, {from, method, path, headers, body}}, waiters} =
            :queue.out(state.waiters)

          GenServer.cast(conn_pid, {:request, from, method, path, headers, body})
          drain_waiters(%{state | waiters: waiters})

        :no_capacity ->
          state
      end
    end
  end
end
