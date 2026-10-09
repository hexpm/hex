defmodule Hex.WorkloadIdentityTest do
  use HexTest.Case

  setup do
    System.put_env("ACTIONS_ID_TOKEN_REQUEST_URL", "http://localhost/token")
    System.put_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "request_token")

    on_exit(fn ->
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_URL")
      System.delete_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
    end)
  end

  describe "auth!/2 defers to configured credentials" do
    test "when the user authenticated" do
      in_tmp(fn ->
        set_home_cwd()

        Hex.OAuth.store_token(%{
          access_token: "token",
          expires_at: System.os_time(:second) + 3600
        })

        assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
      end)
    end

    test "when the hexpm repository has an API key" do
      Hex.State.update!(:repos, &put_in(&1["hexpm"][:api_key], "repo_api_key"))
      assert Hex.WorkloadIdentity.auth!("hexpm", "foo") == []
    end
  end
end
