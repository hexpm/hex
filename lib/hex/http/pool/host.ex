defmodule Hex.HTTP.Pool.Host do
  @moduledoc false

  # Per-host pool. One GenServer per {scheme, host, port, inet}.
  #
  # Connections are opened when a request has to wait because no connection
  # has free capacity, so the number of connections follows how many requests
  # Hex makes at a time:
  #
  #   * While the protocol is unknown at most two connections are opened.
  #   * An HTTP/1 connection serves one request at a time, so a connection is
  #     opened for each waiting request.
  #   * An HTTP/2 connection serves the server's `max_concurrent_streams`
  #     requests at a time, so another connection is only opened when every
  #     connection is full.
  #
  # Dispatch picks the ready conn with the fewest in-flight requests that still
  # has free capacity. Requests are forwarded to the chosen `Conn` via cast
  # with the caller's `from` tuple; the Conn replies directly to the caller and
  # casts `:req_done` back here so we can decrement its in-flight count.
  #
  # A Conn that fails to connect or whose connection closes reports it and is
  # stopped. When a connect fails and no other connection is open or
  # connecting, the waiting requests fail with the connect error so that
  # `Hex.HTTP` can retry them.
  #
  # Connections stay open for the life of the BEAM. Hex runs as a CLI that exits
  # at the end of the Mix task, at which point the supervisor terminates the
  # pool and each Conn closes its socket in terminate/2.

  use GenServer

  alias Hex.HTTP.Pool.Conn

  @unknown_protocol_size 2

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

  def request_to_file(pid, method, path, headers, body, filename, timeout) do
    GenServer.call(pid, {:request_to_file, method, path, headers, body, filename}, timeout)
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
      conns: %{},
      waiters: :queue.new()
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:request, method, path, headers, body}, from, state) do
    dispatch(state, from, {:request, from, method, path, headers, body})
  end

  def handle_call({:request_to_file, method, path, headers, body, filename}, from, state) do
    dispatch(state, from, {:request_to_file, from, method, path, headers, body, filename})
  end

  @impl true
  def handle_cast({:conn_ready, conn_pid, protocol, capacity}, state) do
    state =
      update_conn(state, conn_pid, fn info ->
        %{info | status: :ready, capacity: capacity, in_flight: 0}
      end)

    state = %{state | protocol: state.protocol || protocol}
    {:noreply, state |> drain_waiters() |> start_conns()}
  end

  def handle_cast({:conn_draining, conn_pid}, state) do
    state = update_conn(state, conn_pid, fn info -> %{info | status: :draining} end)
    {:noreply, start_conns(state)}
  end

  def handle_cast({:conn_failed, conn_pid, reason}, state) do
    state = stop_conn(state, conn_pid)

    if Enum.any?(state.conns, fn {_pid, info} -> info.status in [:connecting, :ready] end) do
      {:noreply, state}
    else
      {:noreply, fail_waiters(state, reason)}
    end
  end

  def handle_cast({:conn_closed, conn_pid}, state) do
    {:noreply, state |> stop_conn(conn_pid) |> start_conns()}
  end

  def handle_cast({:requeue, conn_pid, cast_msg}, state) do
    state =
      update_conn(state, conn_pid, fn info ->
        %{info | in_flight: max(info.in_flight - 1, 0)}
      end)

    from = elem(cast_msg, 1)
    waiters = :queue.in_r({from, cast_msg}, state.waiters)
    {:noreply, %{state | waiters: waiters} |> drain_waiters() |> start_conns()}
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
    state = %{state | conns: Map.delete(state.conns, pid)}
    {:noreply, start_conns(state)}
  end

  def handle_info(_, state), do: {:noreply, state}

  ## Conn lifecycle

  defp start_conns(state) do
    waiting = :queue.len(state.waiters)
    connecting = Enum.count(state.conns, fn {_pid, info} -> info.status == :connecting end)

    wanted =
      case state.protocol do
        nil -> min(waiting, @unknown_protocol_size)
        :http1 -> waiting
        :http2 -> min(waiting, 1)
      end

    if wanted > connecting do
      Enum.reduce(1..(wanted - connecting), state, fn _, state -> start_conn(state) end)
    else
      state
    end
  end

  defp start_conn(state) do
    case Conn.start_link({self(), state.key, state.connect_opts}) do
      {:ok, pid} ->
        info = %{status: :connecting, in_flight: 0, capacity: 0}
        %{state | conns: Map.put(state.conns, pid, info)}

      {:error, _reason} ->
        state
    end
  end

  # Requests the host dispatched to the Conn before it processed the Conn's
  # cast reach the Conn before `:stop`, and the Conn hands them back with
  # `:requeue`.
  defp stop_conn(state, pid) do
    GenServer.cast(pid, :stop)
    %{state | conns: Map.delete(state.conns, pid)}
  end

  defp update_conn(state, pid, fun) do
    case Map.get(state.conns, pid) do
      nil -> state
      info -> %{state | conns: Map.put(state.conns, pid, fun.(info))}
    end
  end

  ## Dispatch

  defp pick_conn(state) do
    best =
      state.conns
      |> Enum.filter(fn {_pid, info} ->
        info.status == :ready and info.in_flight < info.capacity
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

  defp dispatch(state, from, cast_msg) do
    case pick_conn(state) do
      {:ok, conn_pid, state} ->
        GenServer.cast(conn_pid, cast_msg)
        {:noreply, state}

      :no_capacity ->
        waiters = :queue.in({from, cast_msg}, state.waiters)
        {:noreply, start_conns(%{state | waiters: waiters})}
    end
  end

  defp drain_waiters(state) do
    if :queue.is_empty(state.waiters) do
      state
    else
      case pick_conn(state) do
        {:ok, conn_pid, state} ->
          {{:value, {_from, cast_msg}}, waiters} = :queue.out(state.waiters)
          GenServer.cast(conn_pid, cast_msg)
          drain_waiters(%{state | waiters: waiters})

        :no_capacity ->
          state
      end
    end
  end

  defp fail_waiters(state, reason) do
    Enum.each(:queue.to_list(state.waiters), fn {from, _cast_msg} ->
      GenServer.reply(from, {:error, reason})
    end)

    %{state | waiters: :queue.new()}
  end
end
