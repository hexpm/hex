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
end
