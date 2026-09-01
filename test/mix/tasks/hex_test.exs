defmodule Mix.Tasks.HexTest do
  use HexTest.Case

  test "run without args shows help" do
    Mix.Tasks.Hex.run([])
    assert_received {:mix_shell, :info, ["Hex is a package manager for the Erlang ecosystem."]}
    assert_received {:mix_shell, :info, ["mix hex.config" <> _]}
  end

  describe "revoke_existing_oauth_tokens/0" do
    test "warns when the server did not accept the revocation" do
      in_tmp(fn ->
        set_home_cwd()
        store_token()
        stub_revocation(500)

        Mix.Tasks.Hex.revoke_existing_oauth_tokens()

        assert Case.shell_output() =~ "Could not revoke the existing authentication token"
      end)
    end

    test "says nothing when the token was revoked" do
      in_tmp(fn ->
        set_home_cwd()
        store_token()
        stub_revocation(200)

        Mix.Tasks.Hex.revoke_existing_oauth_tokens()

        assert Case.shell_output() == ""
      end)
    end
  end

  describe "auth_device/0" do
    test "keeps the session when the device flow fails" do
      in_tmp(fn ->
        set_home_cwd()
        store_token()

        bypass = Bypass.open()
        Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

        Bypass.expect(bypass, fn conn ->
          assert conn.request_path == "/api/oauth/device_authorization"
          Plug.Conn.resp(conn, 500, "")
        end)

        assert Mix.Tasks.Hex.auth_device() == :error

        refute_received {:revoked, _token}
        assert Hex.State.get(:oauth_token).access_token == "token"
        assert Hex.Config.read()[:"$oauth_token"][:access_token] == "token"
      end)
    end

    test "revokes the previous session once the new one is granted" do
      in_tmp(fn ->
        set_home_cwd()
        store_token()
        stub_device_flow()

        assert {:ok, %{access_token: "new_token"}} = Mix.Tasks.Hex.auth_device()

        assert_received {:revoked, "token"}
        assert Hex.State.get(:oauth_token).access_token == "new_token"
        assert Hex.Config.read()[:"$oauth_token"][:access_token] == "new_token"
      end)
    end

    test "prints the verification URL and code without the characters a terminal acts on" do
      in_tmp(fn ->
        set_home_cwd()

        stub_device_flow(
          verification_uri: "hex-test://device\e]0;pwned\a\nYour verification code:",
          user_code: "AB\e[31mCD"
        )

        assert {:ok, _tokens} = Mix.Tasks.Hex.auth_device()

        output = Case.shell_output()

        assert output =~
                 "To authenticate, visit: hex-test://device]0;pwnedYour verification code:"

        assert output =~ "AB[3-1mCD"
        refute output =~ "\e"
      end)
    end
  end

  defp store_token do
    Hex.OAuth.store_token(%{
      access_token: "token",
      refresh_token: "refresh",
      expires_at: System.system_time(:second) + 3600
    })
  end

  # The verification URL is not http(s) so that completing the flow does not
  # open a browser on the machine running the tests.
  defp stub_device_flow(opts \\ []) do
    verification_uri = Keyword.get(opts, :verification_uri, "hex-test://device")
    user_code = Keyword.get(opts, :user_code, "ABCD1234")
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    test_pid = self()

    Bypass.expect(bypass, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)

      case conn.request_path do
        "/api/oauth/device_authorization" ->
          erlang_resp(conn, 200, %{
            "device_code" => "device_code",
            "user_code" => user_code,
            "verification_uri" => verification_uri,
            "verification_uri_complete" => verification_uri,
            "expires_in" => 600,
            "interval" => 0
          })

        "/api/oauth/token" ->
          erlang_resp(conn, 200, %{
            "access_token" => "new_token",
            "refresh_token" => "new_refresh",
            "token_type" => "Bearer",
            "expires_in" => 3600
          })

        "/api/oauth/revoke" ->
          %{"token" => token} = :erlang.binary_to_term(body)
          send(test_pid, {:revoked, token})
          Plug.Conn.resp(conn, 200, "")
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end

  defp stub_revocation(status) do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    Bypass.expect(bypass, fn conn ->
      assert conn.request_path == "/api/oauth/revoke"
      Plug.Conn.resp(conn, status, "")
    end)
  end
end
