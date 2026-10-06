defmodule Hex.HTTPTest do
  use HexTest.Case

  setup do
    on_exit(fn ->
      Enum.each([:http_proxy, :https_proxy, :no_proxy, :http_inet], &Hex.State.put(&1, nil))
      System.delete_env("NETRC")
    end)

    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  defp proxy_auth_header(opts) do
    assert {:http, _host, _port, _proxy_opts} = Keyword.fetch!(opts, :proxy)

    case Keyword.get(opts, :proxy_headers, []) do
      [{"proxy-authorization", "Basic " <> creds}] -> creds
      _ -> nil
    end
  end

  test "proxy_config returns no proxy when none is supplied" do
    assert Hex.HTTP.proxy_config("http://hex.pm") == []
  end

  test "proxy_config encodes http_proxy credentials when supplied" do
    Hex.State.put(:http_proxy, "http://hex:test@example.com")

    opts = Hex.HTTP.proxy_config("http://hex.pm")
    assert proxy_auth_header(opts) == Base.encode64("hex:test")
  end

  test "proxy_config encodes http_proxy credentials when only username supplied" do
    Hex.State.put(:http_proxy, "http://nopass@example.com")

    opts = Hex.HTTP.proxy_config("http://hex.pm")
    assert proxy_auth_header(opts) == Base.encode64("nopass:")
  end

  test "proxy_config encodes credentials when the protocol is https" do
    Hex.State.put(:https_proxy, "https://test:hex@example.com")

    opts = Hex.HTTP.proxy_config("https://hex.pm")
    assert proxy_auth_header(opts) == Base.encode64("test:hex")
  end

  test "proxy_config returns proxy with no auth when no credentials supplied" do
    Hex.State.put(:http_proxy, "http://example.com")

    opts = Hex.HTTP.proxy_config("http://hex.pm")
    assert {:http, "example.com", 80, _proxy_opts} = Keyword.fetch!(opts, :proxy)
    refute Keyword.has_key?(opts, :proxy_headers)
  end

  test "proxy_config connects to the proxy with the request's address family and timeout" do
    Hex.State.put(:http_proxy, "http://example.com")

    opts = Hex.HTTP.proxy_config("http://hex.pm")

    assert Keyword.fetch!(opts, :proxy) ==
             {:http, "example.com", 80,
              [
                transport_opts: [inet4: true, inet6: false, timeout: 15_000],
                tunnel_timeout: 15_000
              ]}
  end

  test "proxy_config uses http_proxy for https requests when https_proxy isn't set" do
    Hex.State.put(:http_proxy, "http://example.com:8080")

    opts = Hex.HTTP.proxy_config("https://hex.pm")
    assert {:http, "example.com", 8080, _proxy_opts} = Keyword.fetch!(opts, :proxy)

    Hex.State.put(:https_proxy, "http://secure.example.com:8443")

    opts = Hex.HTTP.proxy_config("https://hex.pm")
    assert {:http, "secure.example.com", 8443, _proxy_opts} = Keyword.fetch!(opts, :proxy)
  end

  test "proxy_config skips the proxy for hosts matching no_proxy" do
    Hex.State.put(:http_proxy, "http://example.com")

    proxied? = fn no_proxy, url ->
      Hex.State.put(:no_proxy, no_proxy)
      Keyword.has_key?(Hex.HTTP.proxy_config(url), :proxy)
    end

    refute proxied?.("*", "http://hex.pm")
    refute proxied?.("*.hex.pm", "http://repo.hex.pm")
    refute proxied?.("*.hex.pm", "http://hex.pm")
    refute proxied?.(".hex.pm", "http://repo.hex.pm")
    refute proxied?.("hex.pm", "http://repo.hex.pm")
    refute proxied?.("localhost, HEX.PM", "http://hex.pm")
    refute proxied?.("10.", "http://10.1.2.3")
    refute proxied?.("192.168.1.1", "http://192.168.1.1")
    refute proxied?.("fd00:", "http://[fd00::1]")

    assert proxied?.("*.hex.pm", "http://nothex.pm")
    assert proxied?.("hex.pm", "http://nothex.pm")
    assert proxied?.("10.", "http://110.1.2.3")
    assert proxied?.("", "http://hex.pm")
  end

  test "retryable? retries requests the server didn't process for every method" do
    for method <- [:get, :post] do
      assert Hex.HTTP.retryable?(method, %Hex.Mint.HTTPError{reason: :unprocessed})

      assert Hex.HTTP.retryable?(method, %Hex.Mint.HTTPError{
               reason: {:server_closed_request, :refused_stream}
             })

      assert Hex.HTTP.retryable?(method, %Hex.Mint.TransportError{reason: :econnrefused})
      assert Hex.HTTP.retryable?(method, %Hex.Mint.TransportError{reason: :nxdomain})
    end

    assert Hex.HTTP.retryable?(:get, %Hex.Mint.TransportError{reason: :closed})
    refute Hex.HTTP.retryable?(:post, %Hex.Mint.TransportError{reason: :closed})
    refute Hex.HTTP.retryable?(:get, %Hex.Mint.HTTPError{reason: :closed_for_writing})
    refute Hex.HTTP.retryable?(:get, :timeout)
  end

  test "request sends credentials in the URL as basic auth but not to redirects", %{
    bypass: bypass
  } do
    Bypass.expect(bypass, fn conn ->
      case conn.request_path do
        "/redirect" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == [
                   "Basic #{Base.encode64("john:p@ss")}"
                 ]

          conn
          |> Plug.Conn.put_resp_header("location", "http://localhost:#{bypass.port}/final")
          |> Plug.Conn.resp(302, "")

        "/final" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == []
          Plug.Conn.resp(conn, 200, "final")
      end
    end)

    assert {:ok, {200, _headers, "final"}} =
             Hex.HTTP.request(
               :get,
               "http://john:p%40ss@localhost:#{bypass.port}/redirect",
               %{},
               nil
             )
  end

  test "request keeps the last value of a repeated response header" do
    # Cowboy joins repeated response headers, so use a bare-TCP server
    Hex.State.put(:shell_process, self())

    response =
      "HTTP/1.1 200 OK\r\nx-hex-message: \"first\";level=warn\r\n" <>
        "x-hex-message: \"second\";level=warn\r\ncontent-length: 0\r\n\r\n"

    port = start_canned_server(self(), response, :inet)

    assert {:ok, {200, headers, _body}} =
             Hex.HTTP.request(:get, "http://localhost:#{port}", %{}, nil)

    assert headers["x-hex-message"] == "\"second\";level=warn"
    assert_received {:mix_shell, :info, ["API warning: second"]}
  end

  test "requests switch to IPv6 when IPv4 can't reach the host, and stay on IPv6" do
    port = start_canned_server(self(), "HTTP/1.1 200 OK\r\ncontent-length: 4\r\n\r\nipv6", :inet6)

    assert {:ok, {200, _headers, "ipv6"}} =
             Hex.HTTP.request(:get, "http://[::1]:#{port}/", %{}, nil)

    assert Hex.State.get(:http_inet) == :inet6

    assert {:ok, {200, _headers, "ipv6"}} =
             Hex.HTTP.request(:post, "http://[::1]:#{port}/", %{}, {"text/plain", "body"})
  end

  test "requests of every method are retried over IPv6 when IPv4 can't reach the host" do
    port =
      start_canned_server(self(), "HTTP/1.1 201 Created\r\ncontent-length: 4\r\n\r\nipv6", :inet6)

    assert {:ok, {201, _headers, "ipv6"}} =
             Hex.HTTP.request(:post, "http://[::1]:#{port}/", %{}, {"text/plain", "body"})
  end

  test "requests reach a proxy that's only reachable over IPv6" do
    port =
      start_canned_server(
        self(),
        "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nfrom proxy",
        :inet6
      )

    Hex.State.put(:http_proxy, "http://[::1]:#{port}")

    assert {:ok, {200, _headers, "from proxy"}} =
             Hex.HTTP.request(:get, "http://example.invalid/some/path", %{}, nil)

    assert_receive {:proxy_got, request}, 5_000
    assert request =~ ~r{^GET http://example\.invalid/some/path HTTP/1\.1\r\n}
  end

  defp start_canned_server(test_pid, response, family) do
    ip =
      case family do
        :inet -> {127, 0, 0, 1}
        :inet6 -> {0, 0, 0, 0, 0, 0, 0, 1}
      end

    {:ok, listen} = :gen_tcp.listen(0, [:binary, family, active: false, reuseaddr: true, ip: ip])
    {:ok, port} = :inet.port(listen)
    server = spawn(fn -> canned_accept_loop(listen, test_pid, response) end)
    on_exit(fn -> Process.exit(server, :kill) end)
    port
  end

  defp canned_accept_loop(listen, test_pid, response) do
    case :gen_tcp.accept(listen, 5_000) do
      {:ok, socket} ->
        spawn(fn -> handle_canned_conn(socket, test_pid, response) end)
        canned_accept_loop(listen, test_pid, response)

      _ ->
        :gen_tcp.close(listen)
    end
  end

  defp handle_canned_conn(socket, test_pid, response) do
    case read_until_blank_line(socket, "") do
      "" ->
        :gen_tcp.close(socket)

      request ->
        send(test_pid, {:proxy_got, request})
        _ = :gen_tcp.send(socket, response)
        :gen_tcp.close(socket)
    end
  end

  test "x-hex-message" do
    Hex.HTTP.handle_hex_message(~c"\"oops, you done goofed\"")
    refute_received {:mix_shell, _, _}

    Hex.HTTP.handle_hex_message(~c"  \"oops, you done goofed\" ; level = warn")
    assert_received {:mix_shell, :info, ["API warning: oops, you done goofed"]}

    Hex.HTTP.handle_hex_message(~c"\"oops, you done goofed\";level=fatal  ")
    assert_received {:mix_shell, :error, ["API error: oops, you done goofed"]}

    # Headers decoded from a real response arrive as binaries, not charlists
    Hex.HTTP.handle_hex_message("\"oops, you done goofed\" ; level = warn")
    assert_received {:mix_shell, :info, ["API warning: oops, you done goofed"]}
  end

  test "x-hex-message escapes terminal control sequences from the server" do
    Hex.HTTP.handle_hex_message("\"danger\e[31m\u{202E}\";level=fatal")
    assert_received {:mix_shell, :error, ["API error: danger\\e[31m\\u{202E}"]}
  end

  test "request emits the x-hex-message response header", %{bypass: bypass} do
    # handle_response/3 runs inside Task.async, so route shell output to the test process
    Hex.State.put(:shell_process, self())

    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("x-hex-message", "\"heads up\";level=warn")
      |> Plug.Conn.resp(200, "")
    end)

    assert {:ok, {200, _headers, _body}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)

    assert_received {:mix_shell, :info, ["API warning: heads up"]}
  end

  test "request adds no authorization header if none is given and no netrc is found", %{
    bypass: bypass
  } do
    in_tmp(fn ->
      Bypass.expect(bypass, fn conn ->
        assert Plug.Conn.get_req_header(conn, "authorization") == []
        Plug.Conn.resp(conn, 200, "")
      end)

      Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)
    end)
  end

  test "request adds authorization header based on netrc if none is given", %{bypass: bypass} do
    in_tmp(fn ->
      File.write!(".netrc", """
      machine localhost
        login john
        password doe
      """)

      System.put_env("NETRC", Path.join(File.cwd!(), ".netrc"))

      Bypass.expect(bypass, fn conn ->
        assert Plug.Conn.get_req_header(conn, "authorization") == [
                 "Basic #{:base64.encode("john:doe")}"
               ]

        Plug.Conn.resp(conn, 200, "")
      end)

      Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)
    end)
  end

  test "request adds no authorization header based on netrc if authorization is given", %{
    bypass: bypass
  } do
    in_tmp(fn ->
      File.write!(".netrc", """
      machine localhost
        login john
        password doe
      """)

      System.put_env("NETRC", Path.join(File.cwd!(), ".netrc"))

      Bypass.expect(bypass, fn conn ->
        assert Plug.Conn.get_req_header(conn, "authorization") == ["myAuthHeader"]
        Plug.Conn.resp(conn, 200, "")
      end)

      Hex.HTTP.request(
        :get,
        "http://localhost:#{bypass.port}",
        %{"authorization" => "myAuthHeader"},
        nil
      )
    end)
  end

  test "request with Expect 100-continue receives body after 100 response", %{bypass: bypass} do
    body_content = "test request body"

    Bypass.expect(bypass, fn conn ->
      assert ["100-continue"] = Plug.Conn.get_req_header(conn, "expect")

      conn = Plug.Conn.inform(conn, 100, [])

      {:ok, body, conn} = Plug.Conn.read_body(conn)
      assert body == body_content

      Plug.Conn.resp(conn, 201, "success")
    end)

    {:ok, {status, _headers, response_body}} =
      Hex.HTTP.request(
        :post,
        "http://localhost:#{bypass.port}",
        %{"expect" => "100-continue"},
        {"text/plain", body_content}
      )

    assert status == 201
    assert response_body == "success"
  end

  test "informational (1xx) response with headers does not leak into final response", %{
    bypass: bypass
  } do
    Bypass.expect(bypass, fn conn ->
      # 103 Early Hints carries real headers that must NOT appear on the final
      # 200 response.
      conn = Plug.Conn.inform(conn, 103, [{"link", "</style.css>; rel=preload"}])
      Plug.Conn.resp(conn, 200, "body")
    end)

    {:ok, {status, headers, body}} =
      Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)

    assert status == 200
    assert body == "body"
    refute Map.has_key?(headers, "link")
  end

  test "routes plain HTTP requests through http_proxy when configured" do
    # Bypass (Cowboy) rejects absolute-URI request lines, which is exactly
    # what an HTTP/1 proxy client sends. Spin up a bare-TCP listener that
    # records the raw request and returns a canned response, accepting in a
    # loop since the pool can open more than one connection.
    me = self()
    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    proxy = spawn(fn -> proxy_accept_loop(listen, me) end)
    on_exit(fn -> Process.exit(proxy, :kill) end)

    Hex.State.put(:http_proxy, "http://user:pass@localhost:#{port}")

    # `example.invalid` never resolves, but Mint connects to the proxy (not
    # the target) so the request still goes through.
    {:ok, {200, _headers, body}} =
      Hex.HTTP.request(:get, "http://example.invalid/some/path", %{}, nil)

    assert body == "from proxy"
    assert_receive {:proxy_got, request}, 5_000
    assert request =~ ~r{^GET http://example\.invalid/some/path HTTP/1\.1\r\n}
    assert request =~ "proxy-authorization: Basic " <> Base.encode64("user:pass")
  end

  defp proxy_accept_loop(listen, test_pid) do
    case :gen_tcp.accept(listen, 5_000) do
      {:ok, socket} ->
        spawn(fn -> handle_proxy_conn(socket, test_pid) end)
        proxy_accept_loop(listen, test_pid)

      _ ->
        :gen_tcp.close(listen)
    end
  end

  defp handle_proxy_conn(socket, test_pid) do
    case read_until_blank_line(socket, "") do
      "" ->
        :gen_tcp.close(socket)

      request ->
        send(test_pid, {:proxy_got, request})
        _ = :gen_tcp.send(socket, "HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nfrom proxy")
        :gen_tcp.close(socket)
    end
  end

  defp read_until_blank_line(socket, acc) do
    case :gen_tcp.recv(socket, 0, 1_000) do
      {:ok, data} ->
        acc = acc <> data
        if acc =~ "\r\n\r\n", do: acc, else: read_until_blank_line(socket, acc)

      {:error, _} ->
        acc
    end
  end

  test "streamed body with progress callback fires incrementally and delivers full body",
       %{bypass: bypass} do
    # 35_000 bytes = 4 full 10_000-byte chunks + one partial 5_000-byte chunk
    body = String.duplicate("x", 35_000)
    me = self()

    Bypass.expect(bypass, fn conn ->
      {:ok, received, conn} = Plug.Conn.read_body(conn, length: 64_000)
      send(me, {:received, received})
      Plug.Conn.resp(conn, 200, "ok")
    end)

    progress_callback = fn size -> send(me, {:progress, size}) end

    {:ok, {200, _headers, "ok"}} =
      Hex.HTTP.request(
        :post,
        "http://localhost:#{bypass.port}",
        %{},
        {"application/octet-stream", body},
        %{progress_callback: progress_callback}
      )

    assert_received {:received, ^body}
    # Progress callback must fire for each chunk, not just once at completion.
    assert_received {:progress, 10_000}
    assert_received {:progress, 20_000}
    assert_received {:progress, 30_000}
    assert_received {:progress, 35_000}
  end

  test "request with Expect 100-continue stops sending body on error response", %{
    bypass: bypass
  } do
    Bypass.expect(bypass, fn conn ->
      assert ["100-continue"] = Plug.Conn.get_req_header(conn, "expect")
      Plug.Conn.resp(conn, 401, "unauthorized")
    end)

    {:ok, {status, _headers, response_body}} =
      Hex.HTTP.request(
        :post,
        "http://localhost:#{bypass.port}",
        %{"expect" => "100-continue"},
        {"text/plain", "this body should not be sent"}
      )

    assert status == 401
    assert response_body == "unauthorized"
  end

  test "request_to_file downloads to file", %{bypass: bypass} do
    in_tmp(fn ->
      Bypass.expect(bypass, fn conn ->
        Plug.Conn.resp(conn, 200, "file content")
      end)

      filename = Path.join(File.cwd!(), "downloaded.txt")

      {:ok, {status, _headers}} =
        Hex.HTTP.request_to_file(
          :get,
          "http://localhost:#{bypass.port}/file",
          %{},
          nil,
          filename,
          %{}
        )

      assert status == 200
      assert File.read!(filename) == "file content"
    end)
  end
end
