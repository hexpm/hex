defmodule Hex.HTTP.Pool.Conn do
  @moduledoc false

  # A single Mint connection owned by its own GenServer.
  #
  # Because the connection is opened inside this process via `MintHTTP.connect/4`,
  # the socket is owned by us from the start and no `controlling_process/2`
  # transfer is ever needed.
  #
  # After connecting, we report the negotiated protocol and per-conn request
  # capacity back to the parent `Hex.HTTP.Pool.Host`. On HTTP/2 that happens
  # once the server's SETTINGS frame has arrived, so the capacity is the
  # server's `max_concurrent_streams`, and later changes to it are reported
  # with `:conn_capacity`.
  #
  # Requests arrive as casts carrying the caller's `from` tuple. When a request
  # completes we cast `:req_done` to the host, so it can dispatch the next
  # request, and then reply directly to `from` via `GenServer.reply/2`. Callers
  # are monitored and their requests are cancelled when they exit, for example
  # when `Hex.HTTP` gives up on a request after its timeout.
  #
  # A Conn holds a single connection. When connecting fails or the connection
  # closes it tells the host, which opens new connections when requests need
  # them and stops this process with `:stop`. A connection without requests
  # for `idle_timeout` milliseconds is closed.

  use GenServer

  alias Hex.Mint.HTTP, as: MintHTTP
  alias Hex.Mint.HTTP2, as: MintHTTP2

  @default_connect_timeout 30_000

  def start_link({host_pid, key, connect_opts, idle_timeout}) do
    GenServer.start_link(__MODULE__, {host_pid, key, connect_opts, idle_timeout})
  end

  @impl true
  def init({host_pid, key, connect_opts, idle_timeout}) do
    state = %{
      host_pid: host_pid,
      key: key,
      connect_opts: connect_opts,
      idle_timeout: idle_timeout,
      idle_timer: nil,
      conn: nil,
      protocol: nil,
      capacity: 0,
      requests: %{},
      status: :connecting
    }

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state), do: do_connect(state)

  @impl true
  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_cast(
        {:request, _from, _method, _path, _headers, _body} = request,
        %{status: :ready} = state
      ) do
    start_request(state, request, nil)
  end

  def handle_cast(
        {:request_to_file, from, _method, _path, _headers, _body, filename} = request,
        %{status: :ready} = state
      ) do
    case File.open(filename, [:write, :raw, :binary]) do
      {:ok, fd} ->
        start_request(state, request, %{fd: fd, filename: filename})

      {:error, reason} ->
        GenServer.cast(state.host_pid, {:req_done, self(), from})
        GenServer.reply(from, {:error, reason})
        noreply(state)
    end
  end

  # The host dispatched the request before it processed the cast saying this
  # conn stopped taking requests, for example after `req_done` for a response
  # that closed the connection. Hand the request back to be dispatched again.
  def handle_cast(request, state) do
    GenServer.cast(state.host_pid, {:requeue, self(), request})
    noreply(state)
  end

  @impl true
  def handle_info({:DOWN, monitor, :process, _pid, _reason}, state) do
    case Enum.find(state.requests, fn {_ref, req} -> req.monitor == monitor end) do
      {ref, _req} -> noreply(cancel_request(state, ref, nil))
      nil -> noreply(state)
    end
  end

  def handle_info(:handshake_timeout, %{status: :handshaking} = state) do
    noreply(close(state, %Hex.Mint.TransportError{reason: :timeout}))
  end

  def handle_info({:idle_timeout, token}, %{idle_timer: token, requests: requests} = state)
      when requests == %{} do
    noreply(close(state, :idle))
  end

  def handle_info(message, %{conn: conn} = state) when conn != nil do
    case MintHTTP.stream(conn, message) do
      {:ok, conn, responses} ->
        state = %{state | conn: conn}
        state = Enum.reduce(responses, state, &process_response/2)

        state
        |> handshake_done()
        |> update_capacity()
        |> send_bodies()
        |> maybe_draining()
        |> noreply()

      {:error, conn, reason, responses} ->
        state = %{state | conn: conn}
        state = Enum.reduce(responses, state, &process_response/2)
        state = fail_in_flight(state, reason)
        noreply(close(state, reason))

      :unknown ->
        noreply(state)
    end
  end

  def handle_info(_, state), do: noreply(state)

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

  defp start_request(state, request, sink) do
    {method, path, headers, body} = request_args(request)
    from = elem(request, 1)
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

        {pid, _tag} = from
        monitor = Process.monitor(pid)

        req = %{
          from: from,
          monitor: monitor,
          status: nil,
          headers: [],
          data: [],
          sink: sink,
          body: body
        }

        state = %{state | conn: conn, requests: Map.put(state.requests, ref, req)}
        state |> send_bodies() |> maybe_draining() |> noreply()

      # The server lowered max_concurrent_streams after the host dispatched
      # the request, it wasn't sent
      {:error, conn, %Hex.Mint.HTTPError{reason: :too_many_concurrent_requests}} ->
        close_sink(%{sink: sink})
        GenServer.cast(state.host_pid, {:requeue, self(), request})
        noreply(%{state | conn: conn})

      {:error, conn, reason} ->
        close_sink(%{sink: sink})
        GenServer.cast(state.host_pid, {:req_done, self(), from})
        GenServer.reply(from, {:error, reason})
        state = %{state | conn: conn}

        if MintHTTP.open?(conn, :write) do
          noreply(state)
        else
          state |> fail_in_flight(reason) |> close(reason) |> noreply()
        end
    end
  end

  defp request_args({:request, _from, method, path, headers, body}),
    do: {method, path, headers, body}

  defp request_args({:request_to_file, _from, method, path, headers, body, _filename}),
    do: {method, path, headers, body}

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

  defp send_bodies(%{conn: nil} = state), do: state

  defp send_bodies(state) do
    Enum.reduce(state.requests, state, fn
      {ref, %{body: {:sending, body}}}, %{conn: conn} = state when conn != nil ->
        if Map.has_key?(state.requests, ref) do
          send_request_body(state, ref, body)
        else
          state
        end

      _other, state ->
        state
    end)
  end

  defp send_request_body(state, ref, body) do
    case send_body(state.conn, ref, body) do
      {:ok, conn, body} ->
        body = if body, do: {:sending, body}
        state = %{state | conn: conn}
        put_in(state.requests[ref].body, body)

      {:error, conn, reason} ->
        state = %{state | conn: conn}

        # After GOAWAY Mint refuses to send more of the body but the
        # connection still delivers the responses to the other requests
        if MintHTTP.open?(conn, :read) do
          cancel_request(state, ref, reason)
        else
          state |> fail_in_flight(reason) |> close(reason)
        end
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

  ## Connect

  defp do_connect(%{key: {scheme, host, port, _inet, _opts}, connect_opts: opts} = state) do
    # Negotiate HTTP/2 via ALPN when the server supports it; fall back to HTTP/1.
    # Both protocols are equivalent on `mix deps.get` wall time and HTTP/2 uses
    # slightly less CPU (fewer TLS handshakes). Mint's default HTTP/2 receive
    # windows (4 MB per stream, 16 MB per connection) are already tuned for bulk
    # downloads, so no extra tuning is needed here. Hex has no use for server
    # push.
    opts =
      [protocols: [:http1, :http2]]
      |> Keyword.merge(opts)
      |> Keyword.update(:client_settings, [enable_push: false], &[{:enable_push, false} | &1])

    case MintHTTP.connect(scheme, host, port, opts) do
      {:ok, conn} ->
        state = %{state | conn: conn, protocol: MintHTTP.protocol(conn)}

        case state.protocol do
          :http1 ->
            noreply(ready(state, 1))

          :http2 ->
            timeout = get_in(opts, [:transport_opts, :timeout]) || @default_connect_timeout
            Process.send_after(self(), :handshake_timeout, timeout)
            noreply(handshake_done(%{state | status: :handshaking}))
        end

      {:error, reason} ->
        GenServer.cast(state.host_pid, {:conn_failed, self(), reason})
        noreply(%{state | status: :closed})
    end
  end

  # Mint returns from connect before the server's SETTINGS frame has arrived,
  # its connection stays in the `:handshaking` state until then
  defp handshake_done(%{status: :handshaking, conn: %MintHTTP2{state: :handshaking}} = state) do
    state
  end

  # A server that sends GOAWAY right after its SETTINGS frame takes no requests
  defp handshake_done(%{status: :handshaking, conn: conn} = state) do
    if MintHTTP.open?(conn, :write) do
      ready(state, MintHTTP2.get_server_setting(conn, :max_concurrent_streams))
    else
      close(state, %Hex.Mint.HTTPError{module: MintHTTP2, reason: :closed_for_writing})
    end
  end

  defp handshake_done(state), do: state

  defp ready(state, capacity) do
    GenServer.cast(state.host_pid, {:conn_ready, self(), state.protocol, capacity})
    %{state | status: :ready, capacity: capacity}
  end

  defp update_capacity(%{protocol: :http2, status: status, conn: conn} = state)
       when status in [:ready, :draining] and conn != nil do
    case MintHTTP2.get_server_setting(conn, :max_concurrent_streams) do
      capacity when capacity == state.capacity ->
        state

      capacity ->
        GenServer.cast(state.host_pid, {:conn_capacity, self(), capacity})
        %{state | capacity: capacity}
    end
  end

  defp update_capacity(state), do: state

  # A connection that closes before it was ready counts as a failed connect so
  # the host doesn't keep replacing it
  defp close(state, reason) do
    if state.conn, do: safe_close(state.conn)

    if state.status in [:connecting, :handshaking] do
      GenServer.cast(state.host_pid, {:conn_failed, self(), reason})
    else
      GenServer.cast(state.host_pid, {:conn_closed, self()})
    end

    %{state | conn: nil, status: :closed}
  end

  ## Draining (server sent GOAWAY)

  defp maybe_draining(%{status: :ready, conn: conn} = state) do
    if MintHTTP.open?(conn, :write) do
      state
    else
      drain_if_done(stop_accepting(state))
    end
  end

  defp maybe_draining(state), do: drain_if_done(state)

  defp drain_if_done(%{status: :draining, requests: reqs} = state) when reqs == %{} do
    close(state, :closed)
  end

  defp drain_if_done(state), do: state

  defp stop_accepting(state) do
    GenServer.cast(state.host_pid, {:conn_draining, self()})
    %{state | status: :draining}
  end

  ## Idle connections

  defp noreply(state), do: {:noreply, idle_timer(state)}

  defp idle_timer(%{status: :ready, requests: requests, idle_timer: nil} = state)
       when requests == %{} do
    token = make_ref()
    Process.send_after(self(), {:idle_timeout, token}, state.idle_timeout)
    %{state | idle_timer: token}
  end

  defp idle_timer(%{requests: requests, idle_timer: token} = state)
       when requests != %{} and token != nil do
    %{state | idle_timer: nil}
  end

  defp idle_timer(state), do: state

  ## Response handling

  defp process_response({:status, ref, status}, state) do
    # A new status line starts a new response. Reset accumulated headers/data
    # so that 1xx informational responses (100 Continue, 103 Early Hints) don't
    # bleed headers into the final response that follows on the same ref.
    # When streaming to a file, rewind the sink so any partial writes from a
    # prior response on this ref are discarded.
    update_request(state, ref, fn
      %{sink: %{fd: fd}} = req ->
        _ = :file.position(fd, 0)
        _ = :file.truncate(fd)
        reset_response(req, status)

      req ->
        reset_response(req, status)
    end)
  end

  defp process_response({:headers, ref, headers}, state) do
    update_request(state, ref, fn req -> %{req | headers: req.headers ++ headers} end)
  end

  defp process_response({:data, ref, chunk}, state) do
    case Map.fetch(state.requests, ref) do
      {:ok, %{sink: %{fd: fd}}} ->
        case :file.write(fd, chunk) do
          :ok -> state
          {:error, reason} -> cancel_request(state, ref, reason)
        end

      {:ok, req} ->
        put_in(state.requests[ref], %{req | data: [req.data | chunk]})

      :error ->
        state
    end
  end

  defp process_response({:done, ref}, state) do
    case Map.fetch(state.requests, ref) do
      {:ok, req} ->
        state = if req.body, do: close_unfinished_request(state), else: state
        finish_request(state, ref, {:ok, req.status, req.headers, response_body(req)})

      :error ->
        state
    end
  end

  defp process_response({:error, ref, reason}, state) do
    if Map.has_key?(state.requests, ref) do
      finish_request(state, ref, {:error, reason})
    else
      state
    end
  end

  defp process_response(_other, state), do: state

  defp update_request(state, ref, fun) do
    case Map.fetch(state.requests, ref) do
      {:ok, req} -> put_in(state.requests[ref], fun.(req))
      :error -> state
    end
  end

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

  ## Finishing requests

  # Ends a request whose response hasn't finished. On HTTP/2 the stream is
  # reset, an HTTP/1 connection can't be reused in the middle of a response so
  # it's closed. The caller gets `{:error, reason}`, unless reason is nil
  # because the caller is gone.
  defp cancel_request(state, ref, reason) do
    state =
      case state.protocol do
        :http2 ->
          case MintHTTP2.cancel_request(state.conn, ref) do
            {:ok, conn} -> %{state | conn: conn}
            {:error, conn, _reason} -> %{state | conn: conn}
          end

        :http1 ->
          {req, requests} = Map.pop(state.requests, ref)
          state = %{state | requests: requests}
          state = state |> fail_in_flight(reason || :closed) |> close(:closed)
          %{state | requests: %{ref => req}}
      end

    finish_request(state, ref, reason && {:error, reason})
  end

  # `req_done` is cast before the reply so that the host knows the conn has
  # capacity again before the caller sends its next request
  defp finish_request(state, ref, result) do
    {req, requests} = Map.pop(state.requests, ref)
    Process.demonitor(req.monitor, [:flush])

    if result == nil or match?({:error, _}, result), do: close_sink(req)

    GenServer.cast(state.host_pid, {:req_done, self(), req.from})
    if result, do: GenServer.reply(req.from, result)

    %{state | requests: requests}
  end

  defp close_sink(%{sink: %{fd: fd, filename: filename}}) do
    _ = File.close(fd)
    _ = File.rm(filename)
    :ok
  end

  defp close_sink(_), do: :ok

  defp fail_in_flight(state, reason) do
    Enum.reduce(Map.keys(state.requests), state, fn ref, state ->
      finish_request(state, ref, {:error, reason})
    end)
  end

  defp safe_close(conn) do
    try do
      MintHTTP.close(conn)
    catch
      _, _ -> :ok
    end
  end
end
