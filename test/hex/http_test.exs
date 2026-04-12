defmodule Hex.HTTPTest do
  use HexTest.Case

  setup do
    on_exit(fn ->
      Enum.each([:http_proxy, :https_proxy, :no_proxy], &Hex.State.put(&1, nil))
      System.delete_env("NETRC")
    end)

    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  defp proxy_auth_header(opts) do
    case Keyword.get(opts, :proxy) do
      {:http, _host, _port, proxy_opts} ->
        case Keyword.get(proxy_opts, :proxy_headers, []) do
          [{"proxy-authorization", "Basic " <> creds}] -> creds
          _ -> nil
        end

      _ ->
        nil
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
    assert proxy_auth_header(opts) == Base.encode64("nopass")
  end

  test "proxy_config encodes credentials when the protocol is https" do
    Hex.State.put(:https_proxy, "https://test:hex@example.com")

    opts = Hex.HTTP.proxy_config("https://hex.pm")
    assert proxy_auth_header(opts) == Base.encode64("test:hex")
  end

  test "proxy_config returns proxy with no auth when no credentials supplied" do
    Hex.State.put(:http_proxy, "http://example.com")

    opts = Hex.HTTP.proxy_config("http://hex.pm")
    assert {:http, "example.com", 80, proxy_opts} = Keyword.fetch!(opts, :proxy)
    assert Keyword.get(proxy_opts, :proxy_headers, []) == []
  end

  test "x-hex-message" do
    Hex.HTTP.handle_hex_message(~c"\"oops, you done goofed\"")
    refute_received {:mix_shell, _, _}

    Hex.HTTP.handle_hex_message(~c"  \"oops, you done goofed\" ; level = warn")
    assert_received {:mix_shell, :info, ["API warning: oops, you done goofed"]}

    Hex.HTTP.handle_hex_message(~c"\"oops, you done goofed\";level=fatal  ")
    assert_received {:mix_shell, :error, ["API error: oops, you done goofed"]}
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
    # records the raw request and returns a canned response. The pool opens
    # two probe connections so we must accept in a loop and respond to each.
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
end
