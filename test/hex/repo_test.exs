defmodule Hex.RepoTest do
  use HexTest.IntegrationCase

  test "get_package/3" do
    assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

    assert_raise Mix.Error, ~r"Unknown repository \"bad\"", fn ->
      Hex.Repo.get_package("bad", "postgrex", "")
    end
  end

  test "get_tarball/3" do
    assert {:ok, {200, _, _}} = Hex.Repo.get_tarball("hexpm", "postgrex", "0.2.1")

    assert_raise Mix.Error, ~r"Unknown repository \"bad\"", fn ->
      Hex.Repo.get_tarball("bad", "postgrex", "0.2.1")
    end
  end

  test "get public key" do
    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)
    hexpm = Hex.Repo.hexpm_repo()
    repos = put_in(repos["hexpm"].url, "http://localhost:#{bypass.port}")
    Hex.State.put(:repos, repos)

    Bypass.expect(bypass, fn %Plug.Conn{request_path: path} = conn ->
      case path do
        "/public_key" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == ["key"]
          Plug.Conn.resp(conn, 200, hexpm.public_key)

        "/not_found/public_key" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == []
          Plug.Conn.resp(conn, 404, "not found")
      end
    end)

    config = %{
      url: "http://localhost:#{bypass.port}",
      auth_key: "key",
      trusted: true,
      oauth_exchange: false
    }

    assert {:ok, {200, _, public_key}} = Hex.Repo.get_public_key(config)
    assert public_key == hexpm.public_key

    config = %{
      url: "http://localhost:#{bypass.port}/not_found",
      auth_key: "key",
      trusted: false,
      oauth_exchange: false
    }

    assert {:ok, {404, _, "not found"}} = Hex.Repo.get_public_key(config)
  end

  test "fetch_repo/1" do
    assert Hex.Repo.fetch_repo("foo") == :error

    assert {:ok,
            %{
              auth_key: nil,
              public_key: _,
              trusted: true,
              url: "http://localhost:4043/repo"
            }} = Hex.Repo.fetch_repo("hexpm")

    assert {:ok,
            %{
              auth_key: nil,
              public_key: _,
              trusted: true,
              url: "http://localhost:4043/repo/repos/acme"
            }} = Hex.Repo.fetch_repo("hexpm:acme")

    Hex.State.put(:trusted_mirror_url, "http://example.com")
    Hex.State.put(:repos_key, "key")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: true,
              url: "http://example.com"
            }} = Hex.Repo.fetch_repo("hexpm")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: true,
              url: "http://example.com/repos/acme"
            }} = Hex.Repo.fetch_repo("hexpm:acme")

    Hex.State.put(:trusted_mirror_url, nil)
    Hex.State.put(:mirror_url, "http://example.com")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: false,
              url: "http://example.com"
            }} = Hex.Repo.fetch_repo("hexpm")

    assert {:ok,
            %{
              auth_key: "key",
              public_key: _,
              trusted: false,
              url: "http://example.com/repos/acme"
            }} = Hex.Repo.fetch_repo("hexpm:acme")
  end

  test "update_organizations/1 without Hex.State" do
    :ok = Supervisor.terminate_child(Hex.Supervisor, Hex.State)
    :ok = Supervisor.delete_child(Hex.Supervisor, Hex.State)

    repos = %{
      "hexpm" => %{
        url: "http://example.com",
        public_key: "public",
        auth_key: "auth",
        trusted: true
      },
      "hexpm:acme" => %{}
    }

    assert %{
             "hexpm:acme" => %{
               auth_key: "auth",
               public_key: "public",
               trusted: true,
               url: "http://example.com/repos/acme"
             }
           } = Hex.Repo.update_organizations(repos)
  after
    {:ok, _} = Supervisor.start_child(Hex.Supervisor, Hex.State)
  end

  describe "get_package/3" do
    test "from public repo" do
      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "ex_doc", "")
    end

    test "from organization repo" do
      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm:testorg", "foo", "")
    end
  end

  describe "automatic API key to OAuth token exchange" do
    test "automatically exchanges API key for OAuth token when making request" do
      auth =
        HexTest.Hexpm.new_user(
          "repo_oauth_user",
          "repo_oauth@example.com",
          "password",
          "repo_key"
        )

      api_key = auth[:key]

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")
    end

    test "caches OAuth token and reuses it for subsequent requests" do
      auth = HexTest.Hexpm.new_user("cache_user", "cache@example.com", "password", "cache_key")
      api_key = auth[:key]

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      Hex.State.put(:repos, repos)

      repos_before = Hex.State.fetch!(:repos)
      assert Map.get(repos_before["hexpm"], :oauth_token) == nil

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      token_data = repos_after["hexpm"].oauth_token
      assert is_map(token_data)
      assert is_binary(token_data["access_token"])
      assert is_integer(token_data["expires_at"])
      first_token = token_data["access_token"]

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after_2 = Hex.State.fetch!(:repos)
      reused_token = repos_after_2["hexpm"].oauth_token["access_token"]
      assert reused_token == first_token
    end

    test "exchanges for new token when cached token expires" do
      auth = HexTest.Hexpm.new_user("expiry_user", "expiry@example.com", "password", "expiry_key")
      api_key = auth[:key]

      expired_token_data = %{
        "access_token" => "expired_token",
        "expires_at" => System.system_time(:second) - 100
      }

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      repos = put_in(repos["hexpm"], Map.put(repos["hexpm"], :oauth_token, expired_token_data))
      Hex.Config.update_repos(repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      new_token = repos_after["hexpm"].oauth_token["access_token"]
      assert new_token != "expired_token"
      assert is_binary(new_token)
    end

    test "uses API key directly when oauth_exchange is disabled" do
      auth =
        HexTest.Hexpm.new_user(
          "no_oauth_user",
          "no_oauth@example.com",
          "password",
          "no_oauth_key"
        )

      api_key = auth[:key]

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      repos = put_in(repos["hexpm"], Map.put(repos["hexpm"], :oauth_exchange, false))
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      assert Map.get(repos_after["hexpm"], :oauth_token) == nil
    end

    test "handles multiple API keys independently in cache" do
      auth1 =
        HexTest.Hexpm.new_user("multi_user1", "multi1@example.com", "password", "multi_key1")

      auth2 =
        HexTest.Hexpm.new_user("multi_user2", "multi2@example.com", "password", "multi_key2")

      api_key1 = auth1[:key]
      api_key2 = auth2[:key]

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key1)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after1 = Hex.State.fetch!(:repos)
      token1 = repos_after1["hexpm"].oauth_token["access_token"]

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key2)
      repos = put_in(repos["hexpm"], Map.delete(repos["hexpm"], :oauth_token))
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after2 = Hex.State.fetch!(:repos)
      token2 = repos_after2["hexpm"].oauth_token["access_token"]

      assert token1 != token2
    end

    test "raises error when OAuth exchange fails" do
      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, "invalid_key_for_exchange")
      Hex.State.put(:repos, repos)

      assert_raise RuntimeError, ~r/Failed to exchange API key for OAuth token/, fn ->
        Hex.Repo.get_package("hexpm", "postgrex", "")
      end
    end

    test "considers token expired with 5 minute buffer" do
      auth = HexTest.Hexpm.new_user("buffer_user", "buffer@example.com", "password", "buffer_key")
      api_key = auth[:key]

      almost_expired_token = %{
        "access_token" => "almost_expired",
        "expires_at" => System.system_time(:second) + 30
      }

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      repos = put_in(repos["hexpm"], Map.put(repos["hexpm"], :oauth_token, almost_expired_token))
      Hex.Config.update_repos(repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      new_token = repos_after["hexpm"].oauth_token["access_token"]
      assert new_token != "almost_expired"
    end
  end

  describe "OAuth token fallback (device flow)" do
    test "uses OAuth token when no API key is configured" do
      future_time = System.system_time(:second) + 3600

      token_data = %{
        "access_token" => "device_flow_token",
        "refresh_token" => "device_refresh",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(token_data)

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, nil)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")
    end

    test "continues without auth when no API key or OAuth token available" do
      Hex.OAuth.clear_tokens()

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, nil)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")
    end

    test "prefers API key exchange over device flow token" do
      auth =
        HexTest.Hexpm.new_user(
          "priority_user",
          "priority@example.com",
          "password",
          "priority_key"
        )

      api_key = auth[:key]

      future_time = System.system_time(:second) + 3600

      token_data = %{
        "access_token" => "device_flow_token",
        "refresh_token" => "device_refresh",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(token_data)

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      assert repos_after["hexpm"].oauth_token != nil
      assert repos_after["hexpm"].oauth_token["access_token"] != "device_flow_token"
    end

    test "raises error when configured API key exchange fails (no fallback to device flow)" do
      future_time = System.system_time(:second) + 3600

      token_data = %{
        "access_token" => "device_flow_token",
        "refresh_token" => "device_refresh",
        "expires_at" => future_time
      }

      Hex.OAuth.store_token(token_data)

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, "invalid_key")
      Hex.State.put(:repos, repos)

      assert_raise RuntimeError, ~r/Failed to exchange API key for OAuth token/, fn ->
        Hex.Repo.get_package("hexpm", "postgrex", "")
      end
    end
  end
end
