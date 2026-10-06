defmodule Hex.HTTP.Pool.Host do
  @moduledoc false

  # Per-host pool. One GenServer per {scheme, host, port, inet, opts}.
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
  # casts `:req_done` back here. The host tracks which requests each Conn has,
  # so that the callers get an error if the Conn exits without replying.
  # Waiting callers are monitored and their requests dropped when they exit.
  #
  # A Conn that fails to connect or whose connection closes reports it and is
  # stopped. A failed connect isn't retried here: when no other connection is
  # ready the waiting requests fail with the connect error, so that `Hex.HTTP`
  # can retry them.
  #
  # Conns close connections that have been idle for `idle_timeout`
  # milliseconds. The supervisor terminates the pool when Hex exits, at which
  # point each Conn closes its socket in terminate/2.

  use GenServer

  alias Hex.HTTP.Pool.Conn

  @unknown_protocol_size 2

  def start_link({key, connect_opts, opts}) do
    GenServer.start_link(__MODULE__, {key, connect_opts, opts}, Keyword.take(opts, [:name]))
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
  def init({key, connect_opts, opts}) do
    Process.flag(:trap_exit, true)

    state = %{
      key: key,
      connect_opts: connect_opts,
      idle_timeout: Keyword.fetch!(opts, :idle_timeout),
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
    state = update_conn(state, conn_pid, &%{&1 | status: :ready, capacity: capacity})
    state = %{state | protocol: state.protocol || protocol}
    {:noreply, state |> drain_waiters() |> start_conns()}
  end

  def handle_cast({:conn_capacity, conn_pid, capacity}, state) do
    state = update_conn(state, conn_pid, &%{&1 | capacity: capacity})
    {:noreply, drain_waiters(state)}
  end

  def handle_cast({:conn_draining, conn_pid}, state) do
    state = update_conn(state, conn_pid, &%{&1 | status: :draining})
    {:noreply, start_conns(state)}
  end

  def handle_cast({:conn_failed, conn_pid, reason}, state) do
    {:noreply, state |> stop_conn(conn_pid) |> fail_waiters_unless_ready(reason)}
  end

  def handle_cast({:conn_closed, conn_pid}, state) do
    {:noreply, state |> stop_conn(conn_pid) |> start_conns()}
  end

  def handle_cast({:requeue, conn_pid, request}, state) do
    from = elem(request, 1)
    state = update_conn(state, conn_pid, &%{&1 | requests: MapSet.delete(&1.requests, from)})
    {:noreply, state |> enqueue(:front, from, request) |> drain_waiters() |> start_conns()}
  end

  def handle_cast({:req_done, conn_pid, from}, state) do
    state = update_conn(state, conn_pid, &%{&1 | requests: MapSet.delete(&1.requests, from)})
    {:noreply, drain_waiters(state)}
  end

  @impl true
  def handle_info({:DOWN, monitor, :process, _pid, _reason}, state) do
    waiters = :queue.filter(fn {waiter, _from, _request} -> waiter != monitor end, state.waiters)
    {:noreply, %{state | waiters: waiters}}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    case Map.pop(state.conns, pid) do
      {nil, _conns} ->
        {:noreply, state}

      {info, conns} ->
        # Requests the Conn didn't reply to, because it crashed or was killed
        Enum.each(info.requests, &GenServer.reply(&1, {:error, reason}))
        state = %{state | conns: conns}

        case info.status do
          :stopping -> {:noreply, state}
          :connecting -> {:noreply, fail_waiters_unless_ready(state, reason)}
          _ -> {:noreply, start_conns(state)}
        end
    end
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
    case Conn.start_link({self(), state.key, state.connect_opts, state.idle_timeout}) do
      {:ok, pid} ->
        info = %{status: :connecting, capacity: 0, requests: MapSet.new()}
        %{state | conns: Map.put(state.conns, pid, info)}

      {:error, _reason} ->
        state
    end
  end

  # The Conn stays in `conns` until it exits so that requests it hands back
  # with `:requeue`, or never replies to, are accounted for. Requests the host
  # dispatched to the Conn before it processed the Conn's cast reach the Conn
  # before `:stop`, and the Conn hands them back with `:requeue`.
  defp stop_conn(state, pid) do
    GenServer.cast(pid, :stop)
    update_conn(state, pid, &%{&1 | status: :stopping})
  end

  defp update_conn(state, pid, fun) do
    case Map.fetch(state.conns, pid) do
      {:ok, info} -> %{state | conns: Map.put(state.conns, pid, fun.(info))}
      :error -> state
    end
  end

  ## Dispatch

  defp pick_conn(state) do
    state.conns
    |> Enum.filter(fn {_pid, info} ->
      info.status == :ready and MapSet.size(info.requests) < info.capacity
    end)
    |> Enum.min_by(fn {_pid, info} -> MapSet.size(info.requests) end, fn -> nil end)
    |> case do
      nil -> :no_capacity
      {pid, _info} -> {:ok, pid}
    end
  end

  defp dispatch(state, from, request) do
    case pick_conn(state) do
      {:ok, conn_pid} ->
        {:noreply, send_request(state, conn_pid, from, request)}

      :no_capacity ->
        {:noreply, state |> enqueue(:back, from, request) |> start_conns()}
    end
  end

  defp send_request(state, conn_pid, from, request) do
    GenServer.cast(conn_pid, request)
    update_conn(state, conn_pid, &%{&1 | requests: MapSet.put(&1.requests, from)})
  end

  defp enqueue(state, position, {pid, _tag} = from, request) do
    waiter = {Process.monitor(pid), from, request}

    waiters =
      case position do
        :back -> :queue.in(waiter, state.waiters)
        :front -> :queue.in_r(waiter, state.waiters)
      end

    %{state | waiters: waiters}
  end

  defp drain_waiters(state) do
    with {:value, {monitor, from, request}} <- :queue.peek(state.waiters),
         {:ok, conn_pid} <- pick_conn(state) do
      Process.demonitor(monitor, [:flush])
      state = %{state | waiters: :queue.drop(state.waiters)}
      drain_waiters(send_request(state, conn_pid, from, request))
    else
      _ -> state
    end
  end

  defp fail_waiters_unless_ready(state, reason) do
    if Enum.any?(state.conns, fn {_pid, info} -> info.status == :ready end) do
      state
    else
      Enum.each(:queue.to_list(state.waiters), fn {monitor, from, _request} ->
        Process.demonitor(monitor, [:flush])
        GenServer.reply(from, {:error, reason})
      end)

      %{state | waiters: :queue.new()}
    end
  end
end
