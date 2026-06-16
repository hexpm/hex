defmodule Hex.AuthTest do
  use HexTest.Case

  defp get_auth_config(repo) do
    Hex.Auth.callbacks().get_auth_config.(repo)
  end

  defp put_repo(name, config) do
    Hex.State.update!(:repos, &Map.put(&1, name, config))
  end

  describe "get_auth_config/1 callback" do
    test "returns the repo config when no HEX_API_KEY is set" do
      put_repo("acme", %{url: "https://acme.example", auth_key: "repo_key"})
      Hex.State.put(:api_key, nil)

      config = get_auth_config("acme")

      assert config.auth_key == "repo_key"
      refute Map.has_key?(config, :api_key)
    end

    test "surfaces HEX_API_KEY alongside the repo's own credentials" do
      # API and repo credentials authenticate different endpoints, so the env
      # api_key must not displace the repo's auth_key/oauth_token.
      put_repo("acme", %{url: "https://acme.example", auth_key: "repo_key"})
      Hex.State.put(:api_key, "env_api_key")

      config = get_auth_config("acme")

      assert config.api_key == "env_api_key"
      assert config.auth_key == "repo_key"
    end

    test "does not override a per-repo api_key with HEX_API_KEY" do
      put_repo("acme", %{url: "https://acme.example", api_key: "repo_api_key"})
      Hex.State.put(:api_key, "env_api_key")

      assert get_auth_config("acme").api_key == "repo_api_key"
    end

    test "returns just the api_key when the repo is unknown" do
      Hex.State.put(:api_key, "env_api_key")

      assert get_auth_config("does-not-exist") == %{api_key: "env_api_key"}
    end

    test "returns :undefined when the repo is unknown and no HEX_API_KEY is set" do
      Hex.State.put(:api_key, nil)

      assert get_auth_config("does-not-exist") == :undefined
    end
  end
end
