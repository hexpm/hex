defmodule Hex.HTTP.PoolTest do
  use HexTest.Case, async: false

  import Hex.Mint.HTTP2.Frame, only: [goaway: 1, settings: 1, window_update: 1]

  alias HexTest.H2Server

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  defp wait_for_host(key, timeout \\ 500) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait_for_host(key, deadline)
  end

  defp do_wait_for_host(key, deadline) do
    case Registry.lookup(Hex.HTTP.Pool.__registry__(), key) do
      [{pid, _}] ->
        pid

      [] ->
        if System.monotonic_time(:millisecond) >= deadline do
          flunk("no Host registered for #{inspect(key)}")
        else
          Process.sleep(10)
          do_wait_for_host(key, deadline)
        end
    end
  end

  defp wait_until(fun, timeout \\ 1_000) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait_until(fun, deadline)
  end

  defp do_wait_until(fun, deadline) do
    case fun.() do
      {:ok, result} ->
        result

      :retry ->
        if System.monotonic_time(:millisecond) >= deadline do
          flunk("condition not met before timeout")
        else
          Process.sleep(10)
          do_wait_until(fun, deadline)
        end
    end
  end

  test "different inet variants get separate Host pools", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "ok") end)

    url = "http://localhost:#{bypass.port}/"

    pool_opts_inet4 = [
      timeout: 5_000,
      connect_opts: [transport_opts: [inet4: true, inet6: false]]
    ]

    pool_opts_inet6 = [
      timeout: 5_000,
      connect_opts: [transport_opts: [inet4: false, inet6: true]]
    ]

    {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts_inet4)

    pid_inet4 = wait_for_host({:http, "localhost", bypass.port, :inet, []})

    # IPv6 connect likely fails since bypass binds to IPv4; the request will
    # error but a separate Host registration must still have been attempted.
    _ = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts_inet6)

    pid_inet6 = wait_for_host({:http, "localhost", bypass.port, :inet6, []})

    assert pid_inet4 != pid_inet6
  end

  test "Host keeps serving requests after a Conn crashes", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "ok") end)

    url = "http://localhost:#{bypass.port}/"
    pool_opts = [timeout: 5_000]

    {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)

    host_pid = wait_for_host({:http, "localhost", bypass.port, :inet, []})
    [victim] = Map.keys(:sys.get_state(host_pid).conns)

    Process.exit(victim, :kill)

    # Host traps exits and removes the Conn
    wait_until(fn ->
      if victim in Map.keys(:sys.get_state(host_pid).conns), do: :retry, else: {:ok, nil}
    end)

    {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)
  end

  test "requests fail with the connect error when the host refuses connections" do
    {:ok, listen} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(listen)
    :ok = :gen_tcp.close(listen)

    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000]

    for _ <- 1..2 do
      assert {:error, %Hex.Mint.TransportError{reason: :econnrefused}} =
               Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)
    end
  end

  test "HTTP/1 opens a connection for each concurrent request", %{bypass: bypass} do
    {:ok, counter} = Agent.start_link(fn -> {0, 0} end)

    Bypass.expect(bypass, fn conn ->
      Agent.update(counter, fn {current, max} -> {current + 1, max(current + 1, max)} end)
      Process.sleep(200)
      Agent.update(counter, fn {current, max} -> {current - 1, max} end)
      Plug.Conn.resp(conn, 200, "ok")
    end)

    url = "http://localhost:#{bypass.port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http1]]]

    results =
      1..20
      |> Enum.map(fn _ ->
        Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
      end)
      |> Task.await_many(10_000)

    assert Enum.reject(results, &match?({:ok, 200, _, "ok"}, &1)) == []
    assert Agent.get(counter, fn {_current, max} -> max end) == 20
  end

  test "request_to_file streams response body to disk without buffering", %{bypass: bypass} do
    # 5 MB body split into 64 KB chunks — bigger than any reasonable in-memory
    # budget we'd want to pay twice (once in the accumulator, once in File.write).
    chunk = :binary.copy(<<0xAB>>, 65_536)
    chunks = 80
    expected_size = byte_size(chunk) * chunks

    Bypass.expect(bypass, fn conn ->
      conn = Plug.Conn.send_chunked(conn, 200)

      Enum.reduce(1..chunks, conn, fn _, conn ->
        {:ok, conn} = Plug.Conn.chunk(conn, chunk)
        conn
      end)
    end)

    url = "http://localhost:#{bypass.port}/big"
    pool_opts = [timeout: 10_000]

    tmp =
      Path.join(System.tmp_dir!(), "hex-http-pool-stream-#{System.unique_integer([:positive])}")

    try do
      assert {:ok, 200, _headers, nil} =
               Hex.HTTP.Pool.request_to_file(url, "GET", [], nil, tmp, pool_opts)

      assert File.exists?(tmp)
      %File.Stat{size: size} = File.stat!(tmp)
      assert size == expected_size

      # Spot-check a few random offsets to verify bytes round-tripped intact
      # without re-reading the whole file into memory.
      fd = File.open!(tmp, [:read, :raw, :binary])

      try do
        for offset <- [0, div(expected_size, 2), expected_size - 1] do
          {:ok, <<byte>>} = :file.pread(fd, offset, 1)
          assert byte == 0xAB
        end
      after
        File.close(fd)
      end
    after
      _ = File.rm(tmp)
    end
  end

  test "request_to_file truncates prior content on redirect", %{bypass: bypass} do
    # First hit the /redir path, which 302s to /final. The file should contain
    # /final's body only — no trace of any body the redirect response might
    # have had (and the sink must have been reset between iterations).
    Bypass.expect(bypass, fn conn ->
      case conn.request_path do
        "/redir" ->
          conn
          |> Plug.Conn.put_resp_header("location", "/final")
          |> Plug.Conn.resp(302, String.duplicate("X", 4096))

        "/final" ->
          Plug.Conn.resp(conn, 200, "final-body")
      end
    end)

    url = "http://localhost:#{bypass.port}/redir"

    tmp =
      Path.join(System.tmp_dir!(), "hex-http-pool-redirect-#{System.unique_integer([:positive])}")

    try do
      assert {:ok, {200, _headers}} =
               Hex.HTTP.request_to_file("GET", url, %{}, nil, tmp, %{})

      assert File.read!(tmp) == "final-body"
    after
      _ = File.rm(tmp)
    end
  end

  test "requests queue when all Conns are busy and drain as they finish", %{bypass: bypass} do
    # Slow every response so we can saturate a small pool.
    test_pid = self()

    Bypass.expect(bypass, fn conn ->
      send(test_pid, {:bypass_got, conn.request_path})
      Process.sleep(100)
      Plug.Conn.resp(conn, 200, "ok")
    end)

    url_base = "http://localhost:#{bypass.port}"
    pool_opts = [timeout: 5_000]

    # Warm the pool to HTTP/1 (bypass is HTTP/1 only), then spawn more
    # requests than there are connections so they queue while new connections
    # are opened.
    {:ok, 200, _, _} = Hex.HTTP.Pool.request(url_base <> "/warmup", "GET", [], nil, pool_opts)

    count = 12

    tasks =
      for i <- 1..count do
        Task.async(fn ->
          Hex.HTTP.Pool.request(url_base <> "/req#{i}", "GET", [], nil, pool_opts)
        end)
      end

    results = Task.await_many(tasks, 10_000)

    assert length(results) == count
    Enum.each(results, fn {:ok, 200, _, "ok"} -> :ok end)
  end

  test "requests are dispatched again when the server closes connections after each response",
       %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("connection", "close")
      |> Plug.Conn.resp(200, "ok")
    end)

    url = "http://localhost:#{bypass.port}/"
    pool_opts = [timeout: 10_000, connect_opts: [protocols: [:http1]]]

    results =
      1..40
      |> Enum.map(fn _ ->
        Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
      end)
      |> Task.await_many(15_000)

    assert Enum.reject(results, &match?({:ok, 200, _, "ok"}, &1)) == []
  end

  defp chunked_body(body, test_pid) do
    fun = fn
      offset when offset < byte_size(body) ->
        send(test_pid, {:body_chunk, offset})
        size = min(10_000, byte_size(body) - offset)
        {:ok, :binary.part(body, offset, size), offset + size}

      _offset ->
        :eof
    end

    {:stream, fun, 0}
  end

  test "HTTP/2 request bodies larger than the flow-control window are sent in full", %{
    bypass: bypass
  } do
    test_pid = self()

    # Cowboy advertises the default 65_535 byte windows, so this body needs
    # several WINDOW_UPDATE frames from the server before it is fully sent.
    body = :crypto.strong_rand_bytes(1_000_000)

    Bypass.expect(bypass, fn conn ->
      {:ok, received, conn} = Plug.Conn.read_body(conn)
      send(test_pid, {:received, Plug.Conn.get_http_protocol(conn), received})
      Plug.Conn.resp(conn, 201, "ok")
    end)

    url = "http://localhost:#{bypass.port}/upload"
    pool_opts = [timeout: 10_000, connect_opts: [protocols: [:http2]]]

    for request_body <- [body, chunked_body(body, test_pid)] do
      assert {:ok, 201, _headers, "ok"} =
               Hex.HTTP.Pool.request(url, "POST", [], request_body, pool_opts)

      assert_received {:received, :"HTTP/2", ^body}
    end
  end

  for protocol <- [:http1, :http2] do
    test "#{protocol} request with Expect 100-continue sends the body only after 100 Continue",
         %{bypass: bypass} do
      test_pid = self()

      Bypass.expect(bypass, fn conn ->
        case Plug.Conn.get_req_header(conn, "x-otp") do
          [] ->
            Plug.Conn.resp(conn, 401, "otp required")

          ["123456"] ->
            conn = Plug.Conn.inform(conn, 100, [])
            {:ok, received, conn} = Plug.Conn.read_body(conn)
            send(test_pid, {:received, Plug.Conn.get_http_protocol(conn), received})
            Plug.Conn.resp(conn, 201, "created")
        end
      end)

      url = "http://localhost:#{bypass.port}/publish"
      pool_opts = [timeout: 5_000, connect_opts: [protocols: [unquote(protocol)]]]
      headers = [{"expect", "100-continue"}, {"content-length", "11"}]

      assert {:ok, 401, _headers, "otp required"} =
               Hex.HTTP.Pool.request(
                 url,
                 "POST",
                 headers,
                 chunked_body("hello world", test_pid),
                 pool_opts
               )

      refute_received {:body_chunk, _offset}

      assert {:ok, 201, _headers, "created"} =
               Hex.HTTP.Pool.request(
                 url,
                 "POST",
                 [{"x-otp", "123456"} | headers],
                 chunked_body("hello world", test_pid),
                 pool_opts
               )

      assert_received {:body_chunk, 0}

      expected_protocol =
        case unquote(protocol) do
          :http1 -> :"HTTP/1.1"
          :http2 -> :"HTTP/2"
        end

      assert_received {:received, ^expected_protocol, "hello world"}
    end
  end

  test "connect options other than transport options get separate Host pools", %{
    bypass: bypass
  } do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "ok") end)

    url = "http://localhost:#{bypass.port}/"

    for protocol <- [:http1, :http2] do
      pool_opts = [timeout: 5_000, connect_opts: [protocols: [protocol]]]
      assert {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)
    end

    http1 = wait_for_host({:http, "localhost", bypass.port, :inet, [protocols: [:http1]]})
    http2 = wait_for_host({:http, "localhost", bypass.port, :inet, [protocols: [:http2]]})
    assert http1 != http2
  end

  test "sequential HTTP/1 requests reuse one connection", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "ok") end)

    url = "http://localhost:#{bypass.port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http1]]]

    for _ <- 1..20 do
      assert {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)
    end

    host_pid = wait_for_host({:http, "localhost", bypass.port, :inet, [protocols: [:http1]]})
    assert map_size(:sys.get_state(host_pid).conns) == 1
  end

  test "concurrent requests fail with the connect error when the host refuses connections" do
    {:ok, listen} = :gen_tcp.listen(0, [])
    {:ok, port} = :inet.port(listen)
    :ok = :gen_tcp.close(listen)

    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000]

    results =
      1..8
      |> Enum.map(fn _ ->
        Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
      end)
      |> Task.await_many(10_000)

    assert Enum.all?(
             results,
             &match?({:error, %Hex.Mint.TransportError{reason: :econnrefused}}, &1)
           )
  end

  test "concurrent HTTP/1 requests fail with the connect error when connects start failing" do
    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    server =
      Task.async(fn ->
        {:ok, socket} = :gen_tcp.accept(listen)
        {:ok, _request} = :gen_tcp.recv(socket, 0)
        :ok = :gen_tcp.send(socket, "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok")
        :ok = :gen_tcp.close(listen)
        socket
      end)

    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000]

    assert {:ok, 200, _, "ok"} = Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)
    socket = Task.await(server)
    :ok = :gen_tcp.close(socket)

    host_pid = wait_for_host({:http, "localhost", port, :inet, []})

    wait_until(fn ->
      if map_size(:sys.get_state(host_pid).conns) == 0, do: {:ok, nil}, else: :retry
    end)

    results =
      1..8
      |> Enum.map(fn _ ->
        Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
      end)
      |> Task.await_many(10_000)

    assert Enum.all?(
             results,
             &match?({:error, %Hex.Mint.TransportError{reason: :econnrefused}}, &1)
           )
  end

  test "a request is cancelled when its caller exits" do
    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)
    test_pid = self()

    spawn_link(fn ->
      {:ok, socket} = :gen_tcp.accept(listen)
      {:ok, _request} = :gen_tcp.recv(socket, 0)
      send(test_pid, :server_got_request)
      send(test_pid, {:server_recv, :gen_tcp.recv(socket, 0)})
    end)

    url = "http://localhost:#{port}/never"

    tmp =
      Path.join(System.tmp_dir!(), "hex-http-pool-cancel-#{System.unique_integer([:positive])}")

    pool_opts = [timeout: 500]

    task =
      Task.async(fn -> Hex.HTTP.Pool.request_to_file(url, "GET", [], nil, tmp, pool_opts) end)

    assert_receive :server_got_request, 1_000
    assert Task.await(task) == {:error, :timeout}

    assert_receive {:server_recv, {:error, :closed}}, 1_000
    wait_until(fn -> if File.exists?(tmp), do: :retry, else: {:ok, nil} end)
  end

  test "a waiting request is dropped when its caller exits" do
    port = H2Server.start(settings: [max_concurrent_streams: 1], accept: 1)
    url = "http://localhost:#{port}"
    connect_opts = [protocols: [:http2], transport_opts: [timeout: 2_000]]
    pool_opts = [timeout: 5_000, connect_opts: connect_opts]

    first =
      Task.async(fn -> Hex.HTTP.Pool.request(url <> "/first", "GET", [], nil, pool_opts) end)

    assert_receive {:h2_request, conn, stream_id, _headers}, 1_000

    second_opts = Keyword.put(pool_opts, :timeout, 200)

    second =
      Task.async(fn -> Hex.HTTP.Pool.request(url <> "/second", "GET", [], nil, second_opts) end)

    assert Task.await(second) == {:error, :timeout}

    host_pid = wait_for_host({:http, "localhost", port, :inet, [protocols: [:http2]]})

    wait_until(fn ->
      if :queue.is_empty(:sys.get_state(host_pid).waiters), do: {:ok, nil}, else: :retry
    end)

    H2Server.respond(conn, stream_id, 200, "first")
    assert {:ok, 200, _, "first"} = Task.await(first)
    refute_received {:h2_request, _conn, _stream_id, _headers}
  end

  test "HTTP/2 requests stay within the server's max_concurrent_streams" do
    port = H2Server.start(settings: [max_concurrent_streams: 1])
    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]

    tasks =
      for _ <- 1..3 do
        Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
      end

    for _ <- 1..3 do
      assert_receive {:h2_request, conn, stream_id, _headers}, 2_000
      H2Server.respond(conn, stream_id, 200, "ok")
    end

    assert Enum.all?(Task.await_many(tasks), &match?({:ok, 200, _, "ok"}, &1))
  end

  test "HTTP/2 capacity follows changes to the server's max_concurrent_streams" do
    port = H2Server.start()
    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]

    first = Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
    assert_receive {:h2_request, conn, stream_id, _headers}, 1_000
    H2Server.respond(conn, stream_id, 200, "ok")
    assert {:ok, 200, _, "ok"} = Task.await(first)

    host_pid = wait_for_host({:http, "localhost", port, :inet, [protocols: [:http2]]})
    H2Server.send_frames(conn, [settings(params: [max_concurrent_streams: 1])])

    wait_until(fn ->
      case Map.values(:sys.get_state(host_pid).conns) do
        [%{capacity: 1}] -> {:ok, nil}
        _ -> :retry
      end
    end)

    tasks =
      for _ <- 1..3 do
        Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
      end

    for _ <- 1..3 do
      assert_receive {:h2_request, conn, stream_id, _headers}, 2_000
      H2Server.respond(conn, stream_id, 200, "ok")
    end

    assert Enum.all?(Task.await_many(tasks), &match?({:ok, 200, _, "ok"}, &1))
  end

  test "GOAWAY fails the requests the server didn't process and completes the others" do
    port = H2Server.start()
    url = "http://localhost:#{port}"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]

    tasks =
      for path <- ["/a", "/b"], into: %{} do
        {path,
         Task.async(fn -> Hex.HTTP.Pool.request(url <> path, "GET", [], nil, pool_opts) end)}
      end

    assert_receive {:h2_request, conn, first_stream, first_headers}, 1_000
    assert_receive {:h2_request, ^conn, second_stream, second_headers}, 1_000
    assert second_stream > first_stream

    H2Server.send_frames(conn, [
      goaway(last_stream_id: first_stream, error_code: :no_error, debug_data: "")
    ])

    H2Server.respond(conn, first_stream, 200, "ok")

    processed = :proplists.get_value(":path", first_headers)
    unprocessed = :proplists.get_value(":path", second_headers)

    assert {:ok, 200, _, "ok"} = Task.await(tasks[processed])

    assert {:error, %Hex.Mint.HTTPError{reason: :unprocessed}} =
             Task.await(tasks[unprocessed])

    assert_receive {:h2_closed, ^conn}, 1_000
  end

  test "a request body that can't be sent after GOAWAY fails only that request" do
    port = H2Server.start(settings: [initial_window_size: 10])
    url = "http://localhost:#{port}"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]
    body = String.duplicate("x", 1_000)

    upload =
      Task.async(fn -> Hex.HTTP.Pool.request(url <> "/upload", "POST", [], body, pool_opts) end)

    download =
      Task.async(fn -> Hex.HTTP.Pool.request(url <> "/download", "GET", [], nil, pool_opts) end)

    requests =
      for _ <- 1..2, into: %{} do
        assert_receive {:h2_request, conn, stream_id, headers}, 1_000
        {:proplists.get_value(":path", headers), {conn, stream_id}}
      end

    {conn, upload_stream} = requests["/upload"]
    {^conn, download_stream} = requests["/download"]
    assert_receive {:h2_data, ^conn, ^upload_stream, _data}, 1_000

    H2Server.send_frames(conn, [
      goaway(
        last_stream_id: max(upload_stream, download_stream),
        error_code: :no_error,
        debug_data: ""
      ),
      window_update(stream_id: upload_stream, window_size_increment: 1_000)
    ])

    assert {:error, %Hex.Mint.HTTPError{reason: :closed_for_writing}} = Task.await(upload)

    H2Server.respond(conn, download_stream, 200, "ok")
    assert {:ok, 200, _, "ok"} = Task.await(download)
  end

  test "HTTP/2 connections disable server push" do
    port = H2Server.start()
    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]

    task = Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)

    assert_receive {:h2_settings, conn, params}, 1_000
    assert Keyword.fetch!(params, :enable_push) == false

    assert_receive {:h2_request, ^conn, stream_id, _headers}, 1_000
    H2Server.respond(conn, stream_id, 200, "ok")
    assert {:ok, 200, _, "ok"} = Task.await(task)
  end

  test "idle connections are closed" do
    port = H2Server.start()
    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000, idle_timeout: 100, connect_opts: [protocols: [:http2]]]

    task = Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
    assert_receive {:h2_request, conn, stream_id, _headers}, 1_000
    H2Server.respond(conn, stream_id, 200, "ok")
    assert {:ok, 200, _, "ok"} = Task.await(task)

    assert_receive {:h2_closed, ^conn}, 1_000
  end

  test "requests fail when their Conn exits without replying" do
    port = H2Server.start()
    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]

    task = Task.async(fn -> Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts) end)
    assert_receive {:h2_request, _conn, _stream_id, _headers}, 1_000

    host_pid = wait_for_host({:http, "localhost", port, :inet, [protocols: [:http2]]})
    [victim] = Map.keys(:sys.get_state(host_pid).conns)
    Process.exit(victim, :kill)

    assert Task.await(task) == {:error, :killed}
  end

  test "requests fail when the server sends GOAWAY with its SETTINGS" do
    goaway = goaway(last_stream_id: 0, error_code: :no_error, debug_data: "")
    port = H2Server.start(after_settings: [goaway])
    url = "http://localhost:#{port}/"
    pool_opts = [timeout: 5_000, connect_opts: [protocols: [:http2]]]

    assert {:error, %Hex.Mint.HTTPError{reason: :closed_for_writing}} =
             Hex.HTTP.Pool.request(url, "GET", [], nil, pool_opts)

    refute_received {:h2_request, _conn, _stream_id, _headers}
  end
end
