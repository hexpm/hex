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
  def handle_cast({:request, from, method, path, headers, body}, %{ready: true} = state) do
    start_request(state, from, method, path, headers, body, nil)
  end

  def handle_cast({:request, from, _method, _path, _headers, _body}, state) do
    # Host shouldn't dispatch to a non-ready conn; reply defensively.
    GenServer.reply(from, {:error, :disconnected})
    GenServer.cast(state.host_pid, {:req_done, self()})
    {:noreply, state}
  end

  def handle_cast(
        {:request_to_file, from, method, path, headers, body, filename},
        %{ready: true} = state
      ) do
    case File.open(filename, [:write, :raw, :binary]) do
      {:ok, fd} ->
        start_request(state, from, method, path, headers, body, %{fd: fd, filename: filename})

      {:error, reason} ->
        GenServer.reply(from, {:error, reason})
        GenServer.cast(state.host_pid, {:req_done, self()})
        {:noreply, state}
    end
  end

  def handle_cast({:request_to_file, from, _method, _path, _headers, _body, _filename}, state) do
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
        send_bodies(state)

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

  ## Request bodies
  #
  # Bodies are sent with `stream_request_body/3` in chunks no larger than
  # `request_body_window/2`. HTTP/1 has no flow control so the whole body goes
  # out at once. On HTTP/2 we send what the server's window allows and resume
  # from `handle_info/2` once WINDOW_UPDATE frames have been processed.
  #
  # With `expect: 100-continue` the body is held back until the server sends
  # `100 Continue`. If the final response arrives first the body is never sent.

  defp start_request(state, from, method, path, headers, body, sink) do
    {headers, body} = request_body(headers, body)
    mint_body = if body, do: :stream

    case MintHTTP.request(state.conn, method, path, headers, mint_body) do
      {:ok, conn, ref} ->
        body =
          cond do
            body == nil -> nil
            expect_continue?(headers) -> {:continue, body}
            true -> {:sending, body}
          end

        req = %{from: from, status: nil, headers: [], data: [], sink: sink, body: body}
        state = %{state | conn: conn, requests: Map.put(state.requests, ref, req)}
        send_bodies(state)

      {:error, conn, reason} ->
        close_sink(%{sink: sink})
        request_error(from, reason, %{state | conn: conn})
    end
  end

  # A body is `{buffer, next}` where `buffer` is the binary still to be sent
  # and `next` is the `{fun, offset}` producer for the rest, or nil.
  defp request_body(headers, nil), do: {headers, nil}

  defp request_body(headers, {:stream, fun, offset}), do: {headers, {"", {fun, offset}}}

  defp request_body(headers, body) when is_binary(body) do
    headers =
      if List.keymember?(headers, "content-length", 0) do
        headers
      else
        [{"content-length", Integer.to_string(byte_size(body))} | headers]
      end

    {headers, {body, nil}}
  end

  defp expect_continue?(headers) do
    Enum.any?(headers, fn {name, value} ->
      String.downcase(name) == "expect" and String.downcase(value) == "100-continue"
    end)
  end

  defp send_bodies(state) do
    result =
      Enum.reduce_while(state.requests, {:ok, state}, fn
        {ref, %{body: {:sending, body}}}, {:ok, state} ->
          case send_body(state.conn, ref, body) do
            {:ok, conn, body} ->
              body = if body, do: {:sending, body}
              state = %{state | conn: conn}
              {:cont, {:ok, put_in(state.requests[ref].body, body)}}

            {:error, conn, reason} ->
              {:halt, {:error, %{state | conn: conn}, reason}}
          end

        _other, acc ->
          {:cont, acc}
      end)

    case result do
      {:ok, state} ->
        {:noreply, maybe_draining(state)}

      {:error, state, reason} ->
        state
        |> fail_in_flight(reason)
        |> close_and_reconnect()
    end
  end

  defp send_body(conn, ref, {"", nil}) do
    case MintHTTP.stream_request_body(conn, ref, :eof) do
      {:ok, conn} -> {:ok, conn, nil}
      {:error, conn, reason} -> {:error, conn, reason}
    end
  end

  defp send_body(conn, ref, {"", {fun, offset}}) do
    case fun.(offset) do
      :eof ->
        send_body(conn, ref, {"", nil})

      {:ok, chunk, next_offset} ->
        send_body(conn, ref, {IO.iodata_to_binary(chunk), {fun, next_offset}})
    end
  end

  defp send_body(conn, ref, {buffer, next} = body) do
    case MintHTTP.request_body_window(conn, ref) do
      window when window <= 0 ->
        {:ok, conn, body}

      window ->
        size = min(window, byte_size(buffer))
        chunk = :binary.part(buffer, 0, size)
        rest = :binary.part(buffer, size, byte_size(buffer) - size)

        case MintHTTP.stream_request_body(conn, ref, chunk) do
          {:ok, conn} -> send_body(conn, ref, {rest, next})
          {:error, conn, reason} -> {:error, conn, reason}
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
      drain_if_done(stop_accepting(state))
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

  defp stop_accepting(state) do
    GenServer.cast(state.host_pid, {:conn_draining, self()})
    %{state | ready: false}
  end

  ## Response handling

  defp process_response({:status, ref, status}, state) do
    # A new status line starts a new response. Reset accumulated headers/data
    # so that 1xx informational responses (100 Continue, 103 Early Hints) don't
    # bleed headers into the final response that follows on the same ref.
    # When streaming to a file, rewind the sink so any partial writes from a
    # prior response on this ref are discarded.
    update_in(state.requests[ref], fn
      nil ->
        nil

      %{sink: %{fd: fd}} = req ->
        _ = :file.position(fd, 0)
        _ = :file.truncate(fd)
        reset_response(req, status)

      req ->
        reset_response(req, status)
    end)
  end

  defp process_response({:headers, ref, headers}, state) do
    update_in(state.requests[ref], fn req ->
      req && %{req | headers: req.headers ++ headers}
    end)
  end

  defp process_response({:data, ref, chunk}, state) do
    case state.requests[ref] do
      %{sink: %{fd: fd}} ->
        case :file.write(fd, chunk) do
          :ok -> state
          {:error, reason} -> abort_request(state, ref, reason)
        end

      %{} = _req ->
        update_in(state.requests[ref], fn req -> %{req | data: [req.data | chunk]} end)

      nil ->
        state
    end
  end

  defp process_response({:done, ref}, state) do
    case Map.pop(state.requests, ref) do
      {nil, _} ->
        state

      {req, requests} ->
        state = %{state | requests: requests}
        state = if req.body, do: close_unfinished_request(state), else: state
        GenServer.reply(req.from, {:ok, req.status, req.headers, response_body(req)})
        GenServer.cast(state.host_pid, {:req_done, self()})
        state
    end
  end

  defp process_response({:error, ref, reason}, state) do
    case Map.pop(state.requests, ref) do
      {nil, _} ->
        state

      {req, requests} ->
        close_sink(req)
        GenServer.reply(req.from, {:error, reason})
        GenServer.cast(state.host_pid, {:req_done, self()})
        %{state | requests: requests}
    end
  end

  defp process_response(_other, state), do: state

  defp response_body(%{sink: %{fd: fd}}) do
    _ = File.close(fd)
    nil
  end

  defp response_body(req), do: IO.iodata_to_binary(req.data)

  # The response finished before the request body was sent (the server
  # answered without `100 Continue`, or rejected the body early). On HTTP/2
  # Mint has already reset the stream. An HTTP/1 connection is still in the
  # middle of the request so it can't be reused; stop taking requests before
  # `req_done` lets the host dispatch to this conn again.
  defp close_unfinished_request(%{protocol: :http1} = state) do
    {:ok, conn} = MintHTTP.close(state.conn)
    stop_accepting(%{state | conn: conn})
  end

  defp close_unfinished_request(state), do: state

  defp reset_response(req, status) do
    body =
      case {req.body, status} do
        {{:continue, body}, 100} -> {:sending, body}
        {body, _status} -> body
      end

    %{req | status: status, headers: [], data: [], body: body}
  end

  defp abort_request(state, ref, reason) do
    case Map.pop(state.requests, ref) do
      {nil, _} ->
        state

      {req, requests} ->
        close_sink(req)
        GenServer.reply(req.from, {:error, reason})
        GenServer.cast(state.host_pid, {:req_done, self()})
        %{state | requests: requests}
    end
  end

  defp close_sink(%{sink: %{fd: fd, filename: filename}}) do
    _ = File.close(fd)
    _ = File.rm(filename)
    :ok
  end

  defp close_sink(_), do: :ok

  defp fail_in_flight(state, reason) do
    Enum.each(state.requests, fn {_ref, req} ->
      close_sink(req)
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
