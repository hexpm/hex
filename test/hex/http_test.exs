defmodule Hex.HTTPTest do
  use HexTest.Case, async: false

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
end
