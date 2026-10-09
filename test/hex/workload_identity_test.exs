defmodule Hex.WorkloadIdentityTest do
  use HexTest.Case

  setup do
    on_exit(fn ->
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_URL")
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
    end)
  end

  describe "auth!/2 without a workload identity" do
    test "outside GitHub Actions OIDC" do
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
    end

    test "when either GitHub Actions OIDC variable is empty" do
      put_github_oidc_env("")
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []

      put_github_oidc_env("http://localhost/token", "")
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
    end

    test "when HEX_API_KEY is set" do
      put_github_oidc_env("http://localhost/token")
      Hex.State.put(:api_key, "api_key")
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
    end

    test "ignores an empty HEX_API_KEY" do
      original = System.get_env("HEX_API_KEY")

      try do
        System.put_env("HEX_API_KEY", "")
        Hex.State.refresh()
        refute Hex.State.fetch_source!(:api_key) == {:env, "HEX_API_KEY"}
      after
        if original do
          System.put_env("HEX_API_KEY", original)
        else
          System.delete_env("HEX_API_KEY")
        end

        Hex.State.refresh()
        HexTest.Case.reset_state()
      end
    end

    test "when the user authenticated" do
      in_tmp(fn ->
        set_home_cwd()
        put_github_oidc_env("http://localhost/token")

        Hex.OAuth.store_token(%{
          access_token: "token",
          expires_at: System.os_time(:second) + 3600
        })

        assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
      end)
    end

    test "when the hexpm repository has an API key" do
      put_github_oidc_env("http://localhost/token")
      Hex.State.update!(:repos, &put_in(&1["hexpm"][:api_key], "repo_api_key"))
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
    end
  end

  describe "auth!/2 with a workload identity" do
    setup do
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
      put_github_oidc_env("http://localhost:#{bypass.port}/github/token")
      {:ok, bypass: bypass}
    end

    @tag :requires_json
    test "returns OAuth auth for the minted token", %{bypass: bypass} do
      Bypass.expect(bypass, fn conn ->
        case conn.request_path do
          "/api/oidc/audience" ->
            erlang_resp(conn, 200, %{"audience" => "hexpm"})

          "/github/token" ->
            assert conn.query_string == "audience=hexpm"
            Plug.Conn.resp(conn, 200, ~s({"value":"oidc_token"}))

          "/api/oauth/token" ->
            erlang_resp(conn, 200, %{"access_token" => "minted_token"})
        end
      end)

      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == [key: "minted_token", oauth: true]
    end

    @tag :requires_json
    test "raises when GitHub Actions refuses the OIDC token", %{bypass: bypass} do
      Bypass.expect(bypass, fn conn ->
        case conn.request_path do
          "/api/oidc/audience" -> erlang_resp(conn, 200, %{"audience" => "hexpm"})
          "/github/token" -> Plug.Conn.resp(conn, 403, "")
        end
      end)

      message =
        "Workload Identity authentication failed, GitHub Actions refused to issue an OIDC token (HTTP 403)"

      assert_raise Mix.Error, message, fn -> Hex.WorkloadIdentity.auth!("hexpm", "foo") end
    end

    test "raises when Hex does not offer Workload Identity", %{bypass: bypass} do
      Bypass.expect(bypass, fn conn ->
        erlang_resp(conn, 404, %{"status" => 404, "message" => "Not found"})
      end)

      message =
        "Workload Identity authentication failed, could not fetch the OIDC audience from Hex: " <>
          "Not found (HTTP 404)"

      assert_raise Mix.Error, message, fn -> Hex.WorkloadIdentity.auth!("hexpm", "foo") end
    end
  end

  defp put_github_oidc_env(url, token \\ "request_token") do
    System.put_env("ACTIONS_ID_TOKEN_REQUEST_URL", url)
    System.put_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN", token)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
