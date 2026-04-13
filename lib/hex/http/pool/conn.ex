defmodule Hex.HTTP.Pool.Conn do
  @moduledoc false

  # A single Mint connection owned by its own GenServer.
  #
  # Because the connection is opened inside this process via `MintHTTP.connect/4`,
  # the socket is owned by us from the start and no `controlling_process/2`
  # transfer is ever needed.
  #
  # On first successful connect, we report the negotiated protocol and per-conn
  # request capacity back to the parent `Hex.HTTP.Pool.Host`. Requests arrive
  # as casts carrying the caller's `from` tuple; when the request completes we
  # reply directly to that `from` via `GenServer.reply/2` and notify the host
  # so it can decrement its in-flight count for load-based dispatch.

  use GenServer

  alias Hex.Mint.HTTP, as: MintHTTP

  @initial_backoff 1_000
  @max_backoff 30_000

  def start_link({host_pid, key, connect_opts}) do
    GenServer.start_link(__MODULE__, {host_pid, key, connect_opts})
  end

  @impl true
  def init({host_pid, key, connect_opts}) do
    state = %{
      host_pid: host_pid,
      key: key,
      connect_opts: connect_opts,
      conn: nil,
      protocol: nil,
      capacity: 0,
      requests: %{},
      backoff_ms: 0,
      ready: false
    }

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state), do: do_connect(state)

  @impl true
  def handle_cast(
        {:request, from, method, path, headers, {:stream, fun, offset}},
        %{ready: true} = state
      ) do
    case MintHTTP.request(state.conn, method, path, headers, :stream) do
      {:ok, conn, ref} ->
        case stream_body(conn, ref, fun, offset) do
          {:ok, conn} ->
            req = %{from: from, status: nil, headers: [], data: []}
            state = %{state | conn: conn, requests: Map.put(state.requests, ref, req)}
            {:noreply, state}

          {:error, conn, reason} ->
            GenServer.reply(from, {:error, reason})
            GenServer.cast(state.host_pid, {:req_done, self()})
            close_and_reconnect(%{state | conn: conn})
        end

      {:error, conn, reason} ->
        request_error(from, reason, %{state | conn: conn})
    end
  end

  def handle_cast({:request, from, method, path, headers, body}, %{ready: true} = state) do
    case MintHTTP.request(state.conn, method, path, headers, body) do
      {:ok, conn, ref} ->
        req = %{from: from, status: nil, headers: [], data: []}
        state = %{state | conn: conn, requests: Map.put(state.requests, ref, req)}
        {:noreply, state}

      {:error, conn, reason} ->
        request_error(from, reason, %{state | conn: conn})
    end
  end

  def handle_cast({:request, from, _method, _path, _headers, _body}, state) do
    # Host shouldn't dispatch to a non-ready conn; reply defensively.
    GenServer.reply(from, {:error, :disconnected})
    GenServer.cast(state.host_pid, {:req_done, self()})
    {:noreply, state}
  end

  @impl true
  def handle_info(:reconnect, state), do: do_connect(state)

  def handle_info(message, %{conn: conn} = state) when conn != nil do
    case MintHTTP.stream(conn, message) do
      {:ok, conn, responses} ->
        state = %{state | conn: conn}
        state = Enum.reduce(responses, state, &process_response/2)
        {:noreply, maybe_draining(state)}

      {:error, conn, reason, responses} ->
        state = %{state | conn: conn}
        state = Enum.reduce(responses, state, &process_response/2)
        state = fail_in_flight(state, reason)
        close_and_reconnect(state)

      :unknown ->
        {:noreply, state}
    end
  end

  def handle_info(_, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    _ = fail_in_flight(state, :terminated)
    if state.conn, do: safe_close(state.conn)
    :ok
  end

  defp stream_body(conn, ref, fun, offset) do
    case fun.(offset) do
      :eof ->
        MintHTTP.stream_request_body(conn, ref, :eof)

      {:ok, chunk, next_offset} ->
        case MintHTTP.stream_request_body(conn, ref, chunk) do
          {:ok, conn} -> stream_body(conn, ref, fun, next_offset)
          {:error, _conn, _reason} = err -> err
        end
    end
  end

  defp request_error(from, reason, state) do
    GenServer.reply(from, {:error, reason})
    GenServer.cast(state.host_pid, {:req_done, self()})

    if MintHTTP.open?(state.conn, :write) do
      {:noreply, state}
    else
      close_and_reconnect(state)
    end
  end

  ## Connect / reconnect

  defp do_connect(%{key: {scheme, host, port, _inet}, connect_opts: opts} = state) do
    # Negotiate HTTP/2 via ALPN when the server supports it; fall back to HTTP/1.
    # Both protocols are equivalent on `mix deps.get` wall time and HTTP/2 uses
    # slightly less CPU (fewer TLS handshakes). Mint's default HTTP/2 receive
    # windows (4 MB per stream, 16 MB per connection) are already tuned for bulk
    # downloads, so no extra tuning is needed here.
    opts = Keyword.merge([protocols: [:http1, :http2]], opts)

    case MintHTTP.connect(scheme, host, port, opts) do
      {:ok, conn} ->
        protocol = MintHTTP.protocol(conn)
        capacity = compute_capacity(conn, protocol)
        GenServer.cast(state.host_pid, {:conn_ready, self(), protocol, capacity})

        {:noreply,
         %{
           state
           | conn: conn,
             protocol: protocol,
             capacity: capacity,
             ready: true,
             backoff_ms: 0
         }}

      {:error, reason} ->
        schedule_reconnect(reason, %{state | conn: nil, ready: false})
    end
  end

  defp close_and_reconnect(state) do
    if state.conn, do: safe_close(state.conn)
    schedule_reconnect(:closed, %{state | conn: nil, ready: false})
  end

  defp schedule_reconnect(reason, state) do
    GenServer.cast(state.host_pid, {:conn_down, self(), reason})
    backoff = next_backoff(state.backoff_ms)
    Process.send_after(self(), :reconnect, backoff)
    {:noreply, %{state | backoff_ms: backoff}}
  end

  defp next_backoff(0), do: @initial_backoff
  defp next_backoff(n), do: min(n * 2, @max_backoff)

  defp compute_capacity(_conn, :http1), do: 1

  defp compute_capacity(conn, :http2) do
    case Hex.Mint.HTTP2.get_server_setting(conn, :max_concurrent_streams) do
      n when is_integer(n) and n > 0 -> n
      _ -> 100
    end
  end

  ## Draining (server sent GOAWAY)

  defp maybe_draining(%{ready: true, conn: conn} = state) do
    if MintHTTP.open?(conn, :write) do
      state
    else
      GenServer.cast(state.host_pid, {:conn_draining, self()})
      drain_if_done(%{state | ready: false})
    end
  end

  defp maybe_draining(state), do: drain_if_done(state)

  defp drain_if_done(%{ready: false, requests: reqs, conn: conn} = state)
       when reqs == %{} and conn != nil do
    safe_close(conn)
    GenServer.cast(state.host_pid, {:conn_down, self(), :drained})
    Process.send_after(self(), :reconnect, 0)
    %{state | conn: nil, backoff_ms: 0}
  end

  defp drain_if_done(state), do: state

  ## Response handling

  defp process_response({:status, ref, status}, state) do
    # A new status line starts a new response. Reset accumulated headers/data
    # so that 1xx informational responses (100 Continue, 103 Early Hints) don't
    # bleed headers into the final response that follows on the same ref.
    update_in(state.requests[ref], fn req ->
      req && %{req | status: status, headers: [], data: []}
    end)
  end

  defp process_response({:headers, ref, headers}, state) do
    update_in(state.requests[ref], fn req ->
      req && %{req | headers: req.headers ++ headers}
    end)
  end

  defp process_response({:data, ref, chunk}, state) do
    update_in(state.requests[ref], fn req -> req && %{req | data: [req.data | chunk]} end)
  end

  defp process_response({:done, ref}, state) do
    case Map.pop(state.requests, ref) do
      {nil, _} ->
        state

      {req, requests} ->
        body = IO.iodata_to_binary(req.data)
        GenServer.reply(req.from, {:ok, req.status, req.headers, body})
        GenServer.cast(state.host_pid, {:req_done, self()})
        %{state | requests: requests}
    end
  end

  defp process_response({:error, ref, reason}, state) do
    case Map.pop(state.requests, ref) do
      {nil, _} ->
        state

      {req, requests} ->
        GenServer.reply(req.from, {:error, reason})
        GenServer.cast(state.host_pid, {:req_done, self()})
        %{state | requests: requests}
    end
  end

  defp process_response(_other, state), do: state

  defp fail_in_flight(state, reason) do
    Enum.each(state.requests, fn {_ref, req} ->
      GenServer.reply(req.from, {:error, reason})
      GenServer.cast(state.host_pid, {:req_done, self()})
    end)

    %{state | requests: %{}}
  end

  defp safe_close(conn) do
    try do
      MintHTTP.close(conn)
    catch
      _, _ -> :ok
    end
  end
end
