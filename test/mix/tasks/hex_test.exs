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

  defp store_token do
    Hex.OAuth.store_token(%{
      access_token: "token",
      refresh_token: "refresh",
      expires_at: System.system_time(:second) + 3600
    })
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
