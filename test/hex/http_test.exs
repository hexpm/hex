defmodule Hex.HTTPTest do
  use HexTest.Case

  setup do
    on_exit(fn ->
      Enum.map([:http_proxy, :https_proxy], &Hex.State.put(&1, nil))

      Enum.map([:proxy, :https_proxy], fn opt ->
        :httpc.set_options([{opt, {{~c"localhost", 80}, [~c"localhost"]}}], :hex)
      end)

      System.delete_env("NETRC")
    end)

    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  test "proxy_config returns no credentials when no proxy supplied" do
    assert Hex.HTTP.proxy_config("http://hex.pm") == []
  end

  test "proxy_config returns http_proxy credentials when supplied" do
    Hex.State.put(:http_proxy, "http://hex:test@example.com")

    assert Hex.HTTP.proxy_config("http://hex.pm") == [proxy_auth: {~c"hex", ~c"test"}]
  end

  test "proxy_config returns http_proxy credentials when only username supplied" do
    Hex.State.put(:http_proxy, "http://nopass@example.com")

    assert Hex.HTTP.proxy_config("http://hex.pm") == [proxy_auth: {~c"nopass", ~c""}]
  end

  test "proxy_config returns credentials when the protocol is https" do
    Hex.State.put(:https_proxy, "https://test:hex@example.com")

    assert Hex.HTTP.proxy_config("https://hex.pm") == [proxy_auth: {~c"test", ~c"hex"}]
  end

  test "proxy_config returns empty list when no credentials supplied" do
    Hex.State.put(:http_proxy, "http://example.com")

    assert Hex.HTTP.proxy_config("http://hex.pm") == []
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
    # Test that httpc handles 100-continue flow correctly
    body_content = "test request body"

    Bypass.expect(bypass, fn conn ->
      # Verify the Expect header is present
      assert ["100-continue"] = Plug.Conn.get_req_header(conn, "expect")

      # Send 100 Continue informational response
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

  test "request with Expect 100-continue stops sending body on error response", %{
    bypass: bypass
  } do
    # Test that when server responds with error before 100, body is not sent
    # Note: This is handled by httpc automatically - if server responds with
    # error status instead of 100 Continue, httpc won't send the body

    Bypass.expect(bypass, fn conn ->
      # Verify the Expect header is present
      assert ["100-continue"] = Plug.Conn.get_req_header(conn, "expect")

      # Immediately respond with 401 Unauthorized without reading body
      # httpc should NOT send the body when it receives this error
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

  @max_body_size 16 * 1024 * 1024
  @limited %{max_body_size: @max_body_size}

  test "request decompresses gzip-encoded responses", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-encoding", "gzip")
      |> Plug.Conn.resp(200, :zlib.gzip("body"))
    end)

    assert {:ok, {200, _headers, "body"}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)
  end

  test "request rejects responses larger than max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      Plug.Conn.resp(conn, 200, :binary.copy(<<0>>, @max_body_size + 1))
    end)

    assert Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited) ==
             {:error, :body_too_big}
  end

  test "request doesn't limit responses without max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      Plug.Conn.resp(conn, 200, :binary.copy(<<0>>, @max_body_size + 1))
    end)

    assert {:ok, {200, _headers, body}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)

    assert byte_size(body) == @max_body_size + 1
  end

  test "request rejects chunked responses larger than max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      conn = Plug.Conn.send_chunked(conn, 200)
      chunk = :binary.copy(<<0>>, 1024 * 1024)

      Enum.reduce_while(1..17, conn, fn _, conn ->
        case Plug.Conn.chunk(conn, chunk) do
          {:ok, conn} -> {:cont, conn}
          {:error, _reason} -> {:halt, conn}
        end
      end)
    end)

    assert Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited) ==
             {:error, :body_too_big}
  end

  test "request rejects gzip-encoded responses that decompress to more than max_body_size", %{
    bypass: bypass
  } do
    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-encoding", "gzip")
      |> Plug.Conn.resp(200, :zlib.gzip(:binary.copy(<<0>>, @max_body_size + 1)))
    end)

    assert Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited) ==
             {:error, :body_too_big}
  end

  test "request decompresses concatenated and empty gzip members", %{bypass: bypass} do
    body = :zlib.gzip("") <> :zlib.gzip("a") <> :zlib.gzip("") <> :zlib.gzip("bc")

    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-encoding", "gzip")
      |> Plug.Conn.resp(200, body)
    end)

    assert {:ok, {200, _headers, "abc"}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited)
  end

  test "request accepts gzip members that decompress to exactly max_body_size", %{
    bypass: bypass
  } do
    half = :binary.copy(<<0>>, div(@max_body_size, 2))

    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-encoding", "gzip")
      |> Plug.Conn.resp(200, :zlib.gzip(half) <> :zlib.gzip(half))
    end)

    assert {:ok, {200, _headers, body}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited)

    assert byte_size(body) == @max_body_size
  end

  test "request decompresses responses past 16 MiB without max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-encoding", "gzip")
      |> Plug.Conn.resp(200, :zlib.gzip(:binary.copy(<<0>>, @max_body_size + 1)))
    end)

    assert {:ok, {200, _headers, body}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)

    assert byte_size(body) == @max_body_size + 1
  end

  test "repository responses are limited to 16 MiB", %{bypass: bypass} do
    Hex.State.update!(:repos, &put_in(&1["hexpm"].url, "http://localhost:#{bypass.port}"))

    Bypass.expect(bypass, "GET", "/docs/foo-1.0.0.tar.gz", fn conn ->
      Plug.Conn.resp(conn, 200, :binary.copy(<<0>>, @max_body_size))
    end)

    Bypass.expect(bypass, "GET", "/docs/foo-1.0.1.tar.gz", fn conn ->
      Plug.Conn.resp(conn, 200, :binary.copy(<<0>>, @max_body_size + 1))
    end)

    assert {:ok, {200, _headers, body}} = Hex.Repo.get_docs("hexpm", "foo", "1.0.0")
    assert byte_size(body) == @max_body_size
    assert Hex.Repo.get_docs("hexpm", "foo", "1.0.1") == {:error, :body_too_big}
  end

  test "API responses aren't limited to 16 MiB", %{bypass: bypass} do
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    description = :binary.copy("a", @max_body_size + 1)

    Bypass.expect(bypass, "GET", "/api/packages/foo", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
      |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(%{"description" => description}))
    end)

    assert {:ok, {200, _headers, %{"description" => ^description}}} =
             Hex.API.Package.get(nil, "foo")
  end

  test "API responses are limited to 64 MiB after gzip decompression", %{bypass: bypass} do
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    Bypass.expect(bypass, "GET", "/api/packages/foo", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
      |> Plug.Conn.put_resp_header("content-encoding", "gzip")
      |> Plug.Conn.resp(200, :zlib.gzip(:binary.copy(<<0>>, 64 * 1024 * 1024 + 1)))
    end)

    assert Hex.API.Package.get(nil, "foo") == {:error, :body_too_big}
  end

  test "request_to_file returns the response headers", %{bypass: bypass} do
    in_tmp(fn ->
      Bypass.expect(bypass, fn conn ->
        conn
        |> Plug.Conn.put_resp_header("etag", "\"abc\"")
        |> Plug.Conn.resp(200, "file content")
      end)

      filename = Path.join(File.cwd!(), "downloaded.txt")

      assert {:ok, {200, %{"etag" => "\"abc\""}}} =
               Hex.HTTP.request_to_file(
                 :get,
                 "http://localhost:#{bypass.port}/file",
                 %{},
                 nil,
                 filename,
                 %{}
               )
    end)
  end

  test "request_to_file doesn't write the body of other responses", %{bypass: bypass} do
    in_tmp(fn ->
      Bypass.expect(bypass, fn conn ->
        Plug.Conn.resp(conn, 404, "not found")
      end)

      filename = Path.join(File.cwd!(), "downloaded.txt")

      assert {:ok, {404, _headers}} =
               Hex.HTTP.request_to_file(
                 :get,
                 "http://localhost:#{bypass.port}/file",
                 %{},
                 nil,
                 filename,
                 %{}
               )

      refute File.exists?(filename)
    end)
  end

  test "request_to_file rejects responses larger than max_body_size and removes the file", %{
    bypass: bypass
  } do
    in_tmp(fn ->
      Bypass.expect(bypass, fn conn ->
        Plug.Conn.resp(conn, 200, :binary.copy(<<0>>, @max_body_size + 1))
      end)

      filename = Path.join(File.cwd!(), "downloaded.txt")

      assert Hex.HTTP.request_to_file(
               :get,
               "http://localhost:#{bypass.port}/file",
               %{},
               nil,
               filename,
               @limited
             ) == {:error, :body_too_big}

      refute File.exists?(filename)
    end)
  end

  @tag :requires_httpc_max_body_size
  test "request rejects other responses larger than max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      Plug.Conn.resp(conn, 500, :binary.copy(<<0>>, @max_body_size + 1))
    end)

    assert Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited) ==
             {:error, :body_too_big}
  end

  @tag :requires_httpc_max_body_size
  test "request rejects chunked other responses larger than max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      conn = Plug.Conn.send_chunked(conn, 500)
      chunk = :binary.copy(<<0>>, 1024 * 1024)

      Enum.reduce_while(1..17, conn, fn _, conn ->
        case Plug.Conn.chunk(conn, chunk) do
          {:ok, conn} -> {:cont, conn}
          {:error, _reason} -> {:halt, conn}
        end
      end)
    end)

    assert Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited) ==
             {:error, :body_too_big}
  end

  @tag :requires_httpc_max_body_size
  test "request_to_file rejects other responses larger than max_body_size", %{bypass: bypass} do
    in_tmp(fn ->
      Bypass.expect(bypass, fn conn ->
        Plug.Conn.resp(conn, 404, :binary.copy(<<0>>, @max_body_size + 1))
      end)

      filename = Path.join(File.cwd!(), "downloaded.txt")

      assert Hex.HTTP.request_to_file(
               :get,
               "http://localhost:#{bypass.port}/file",
               %{},
               nil,
               filename,
               @limited
             ) == {:error, :body_too_big}

      refute File.exists?(filename)
    end)
  end

  @tag :requires_httpc_max_body_size
  test "request reports response headers larger than httpc's limit", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("x-large", :binary.copy("a", 20_000))
      |> Plug.Conn.resp(200, "")
    end)

    assert Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil) ==
             {:error, :header_too_big}
  end

  test "request reads other responses whole without max_body_size", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      Plug.Conn.resp(conn, 500, :binary.copy(<<0>>, @max_body_size + 1))
    end)

    assert {:ok, {500, _headers, body}} =
             Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil)

    assert byte_size(body) == @max_body_size + 1
  end

  test "request only passes options to httpc that it knows", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "body") end)
    level = Logger.level()
    Logger.configure(level: :info)

    try do
      log =
        ExUnit.CaptureLog.capture_log(fn ->
          assert {:ok, {200, _headers, "body"}} =
                   Hex.HTTP.request(:get, "http://localhost:#{bypass.port}", %{}, nil, @limited)
        end)

      refute log =~ "Invalid option"
    after
      Logger.configure(level: level)
    end
  end

  test "httpc_max_body_size?/1 is true for the inets versions with the max_body_size option" do
    for vsn <- ~w(9.3.2.7 9.6.2.3 9.7.2 9.8) do
      assert Hex.HTTP.httpc_max_body_size?(String.to_charlist(vsn)), vsn
    end

    for vsn <- ~w(7.5.3.4 8.3.1.5 9.1.0.7 9.2 9.3.2 9.3.2.6 9.4.2 9.5 9.6.2 9.6.2.2 9.7 9.7.1) do
      refute Hex.HTTP.httpc_max_body_size?(String.to_charlist(vsn)), vsn
    end
  end

  test "request_to_file removes the partial file when the request times out" do
    in_tmp(fn ->
      {:ok, listener} = :gen_tcp.listen(0, [:binary, active: false])
      {:ok, port} = :inet.port(listener)

      # Sends part of the body and then stalls
      spawn_link(fn ->
        {:ok, socket} = :gen_tcp.accept(listener)
        {:ok, _request} = :gen_tcp.recv(socket, 0)
        :ok = :gen_tcp.send(socket, "HTTP/1.1 200 OK\r\ncontent-length: 100\r\n\r\npartial")
        Process.sleep(:infinity)
      end)

      filename = Path.join(File.cwd!(), "downloaded.txt")

      assert Hex.HTTP.request_to_file(
               :get,
               "http://localhost:#{port}/file",
               %{},
               nil,
               filename,
               %{timeout: 200}
             ) == {:error, :timeout}

      refute File.exists?(filename)
    end)
  end
end
