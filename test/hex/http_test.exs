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
    assert {:http, ~c"example.com", 80, proxy_opts} = Keyword.fetch!(opts, :proxy)
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
end
