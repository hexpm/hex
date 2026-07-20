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

  test "does not send OAuth token fallback to untrusted hexpm mirror" do
    bypass = Bypass.open()
    hexpm = Hex.Repo.default_hexpm_repo()

    Hex.OAuth.store_token(%{
      access_token: "device_flow_token",
      refresh_token: "device_refresh",
      expires_at: System.system_time(:second) + 3600
    })

    Hex.State.put(:mirror_url, "http://localhost:#{bypass.port}")

    Bypass.expect(bypass, fn %Plug.Conn{request_path: "/public_key"} = conn ->
      assert Plug.Conn.get_req_header(conn, "authorization") == []
      Plug.Conn.resp(conn, 200, hexpm.public_key)
    end)

    repo = Hex.Repo.get_repo("hexpm")
    assert repo.trusted == false

    assert {:ok, {200, _, public_key}} = Hex.Repo.get_public_key(repo)
    assert public_key == hexpm.public_key
  end

  test "add repo persists oauth_exchange through config round-trip" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      Mix.Tasks.Hex.Repo.run(["add", "myrepo", "http://example.com", "--auth-key", "mykey"])

      # Reload config from disk to simulate a fresh session
      config = Hex.Config.read()
      repos = Hex.Config.read_repos(config)

      assert repos["myrepo"].auth_key == "mykey"
      assert repos["myrepo"].oauth_exchange == false
    end)
  end

  test "auth_key_owner persists through config round-trip" do
    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      repos = Hex.State.fetch!(:repos)

      org_repo = %{auth_key: "org_key_value", auth_key_owner: :organization}

      repos
      |> Map.put("hexpm:roundtriporg", org_repo)
      |> Hex.Config.update_repos()

      # Reload config from disk to simulate a fresh session
      config = Hex.Config.read()
      repos = Hex.Config.read_repos(config)

      assert repos["hexpm:roundtriporg"].auth_key == "org_key_value"
      assert repos["hexpm:roundtriporg"].auth_key_owner == :organization
    end)
  end

  test "warns about deprecation when a stored key authenticates to an organization repository" do
    Hex.Server.reset()

    repos = Hex.State.fetch!(:repos)
    hexpm = repos["hexpm"]

    org_repo = %{
      url: hexpm.url <> "/repos/storedkeyorg",
      public_key: hexpm.public_key,
      auth_key: "stored_key_value",
      oauth_exchange: true,
      trusted: true
    }

    Hex.State.put(:repos, Map.put(repos, "hexpm:storedkeyorg", org_repo))

    # The exchange outcome is irrelevant; the warning is emitted before the
    # stored key is exchanged.
    try do
      Hex.Repo.get_package("hexpm:storedkeyorg", "pkg", "")
    rescue
      _ -> :ok
    end

    output = Case.shell_output()
    assert output =~ "stored key is deprecated and will stop working in Hex 2.6"
    assert output =~ "mix hex.user auth"

    assert output =~
             "re-run `mix hex.organization auth storedkeyorg --key KEY` on this Hex version"
  end

  test "warns about deprecation when a user-owned key authenticates to an organization repository" do
    Hex.Server.reset()

    repos = Hex.State.fetch!(:repos)
    hexpm = repos["hexpm"]

    org_repo = %{
      url: hexpm.url <> "/repos/userkeyorg",
      public_key: hexpm.public_key,
      auth_key: "user_key_value",
      auth_key_owner: :user,
      oauth_exchange: true,
      trusted: true
    }

    Hex.State.put(:repos, Map.put(repos, "hexpm:userkeyorg", org_repo))

    try do
      Hex.Repo.get_package("hexpm:userkeyorg", "pkg", "")
    rescue
      _ -> :ok
    end

    output = Case.shell_output()

    assert output =~
             "key owned by your user account is deprecated and will stop working in Hex 2.6"

    assert output =~ "mix hex.user auth"
    refute output =~ "re-run"
  end

  test "does not warn when an organization-owned key authenticates to an organization repository" do
    Hex.Server.reset()

    repos = Hex.State.fetch!(:repos)
    hexpm = repos["hexpm"]

    org_repo = %{
      url: hexpm.url <> "/repos/orgkeyorg",
      public_key: hexpm.public_key,
      auth_key: "org_key_value",
      auth_key_owner: :organization,
      oauth_exchange: true,
      trusted: true
    }

    Hex.State.put(:repos, Map.put(repos, "hexpm:orgkeyorg", org_repo))

    try do
      Hex.Repo.get_package("hexpm:orgkeyorg", "pkg", "")
    rescue
      _ -> :ok
    end

    output = Case.shell_output()
    refute output =~ "deprecated"
  end

  test "warns about deprecation when HEX_REPOS_KEY authenticates to an organization repository" do
    Hex.Server.reset()

    repos = Hex.State.fetch!(:repos)
    hexpm = repos["hexpm"]
    Hex.State.put(:repos_key, "repos_key_value")

    org_repo = %{
      url: hexpm.url <> "/repos/reposkeyorg",
      public_key: hexpm.public_key,
      auth_key: "repos_key_value",
      oauth_exchange: true,
      trusted: true
    }

    Hex.State.put(:repos, Map.put(repos, "hexpm:reposkeyorg", org_repo))

    try do
      Hex.Repo.get_package("hexpm:reposkeyorg", "pkg", "")
    rescue
      _ -> :ok
    end

    output = Case.shell_output()

    assert output =~
             "the reposkeyorg repository with HEX_REPOS_KEY is deprecated and will stop working in Hex 2.6"
  end

  test "does not warn for the base hexpm repository authenticated with HEX_REPOS_KEY" do
    Hex.Server.reset()

    auth =
      HexTest.Hexpm.new_user(
        "repos_key_user",
        "repos_key@example.com",
        "password",
        "repos_key_key"
      )

    # HEX_REPOS_KEY still authenticates the base hexpm repo (and trusted mirrors)
    Hex.State.put(:repos_key, auth[:key])

    try do
      Hex.Repo.get_package("hexpm", "postgrex", "")
    rescue
      _ -> :ok
    end

    output = Case.shell_output()
    refute output =~ "HEX_REPOS_KEY"
    refute output =~ "deprecated"
  end

  test "does not attempt oauth exchange when oauth_exchange is not set" do
    # Simulates a repo configured before v2.4.0 (no oauth_exchange key).
    # If oauth exchange were attempted with an invalid key it would raise.
    auth =
      HexTest.Hexpm.new_user(
        "no_oauth_key_user",
        "no_oauth_key@example.com",
        "password",
        "no_oauth_key_key"
      )

    repos = Hex.State.fetch!(:repos)
    hexpm = Map.delete(repos["hexpm"], :oauth_exchange)
    hexpm = %{hexpm | auth_key: auth[:key]}
    Hex.State.put(:repos, %{repos | "hexpm" => hexpm})

    assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

    repos_after = Hex.State.fetch!(:repos)
    assert Map.get(repos_after["hexpm"], :oauth_token) == nil
  end

  test "non-hexpm repo with oauth_exchange: false uses API key directly and does not exchange" do
    bypass = Bypass.open()

    Bypass.expect(bypass, fn %Plug.Conn{request_path: path} = conn ->
      case path do
        "/public_key" ->
          assert Plug.Conn.get_req_header(conn, "authorization") == ["rawkey"]
          Plug.Conn.resp(conn, 200, "fake_public_key_body")

        "/oauth/token" ->
          flunk("OAuth exchange must not be attempted when oauth_exchange is false")
      end
    end)

    myrepo_config = %{
      url: "http://localhost:#{bypass.port}",
      public_key: nil,
      auth_key: "rawkey",
      trusted: true,
      oauth_exchange: false,
      oauth_exchange_url: "http://localhost:#{bypass.port}"
    }

    assert {:ok, {200, _, "fake_public_key_body"}} = Hex.Repo.get_public_key(myrepo_config)
  end

  test "non-hexpm repo with oauth_exchange not true does not attempt API key exchange" do
    bypass = Bypass.open()
    test_pid = self()

    Bypass.expect(bypass, fn %Plug.Conn{request_path: path} = conn ->
      case path do
        "/public_key" ->
          Plug.Conn.resp(conn, 200, "fake_public_key_body")

        "/oauth/token" ->
          send(test_pid, :exchange_attempted)
          Plug.Conn.resp(conn, 500, "")
      end
    end)

    myrepo_config = %{
      url: "http://localhost:#{bypass.port}",
      public_key: nil,
      auth_key: "rawkey",
      trusted: true,
      oauth_exchange: nil,
      oauth_exchange_url: "http://localhost:#{bypass.port}"
    }

    assert {:ok, {200, _, _}} = Hex.Repo.get_public_key(myrepo_config)

    refute_received :exchange_attempted
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
              oauth_exchange: true,
              public_key: _,
              trusted: true,
              url: "http://localhost:4043/repo"
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
              oauth_exchange: true,
              public_key: _,
              trusted: true,
              url: "http://example.com"
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
              oauth_exchange: true,
              public_key: _,
              trusted: false,
              url: "http://example.com"
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
        oauth_exchange: true,
        trusted: true
      },
      "hexpm:acme" => %{}
    }

    assert %{
             "hexpm:acme" => %{
               auth_key: "auth",
               oauth_exchange: true,
               public_key: "public",
               trusted: true,
               url: "http://example.com"
             }
           } = Hex.Repo.update_organizations(repos)
  after
    {:ok, _} = Supervisor.start_child(Hex.Supervisor, Hex.State)
  end

  test "update_organizations/1 normalizes a legacy baked-in org URL" do
    repos = Hex.State.fetch!(:repos)
    source_url = repos["hexpm"].url
    repos = Map.put(repos, "hexpm:acme", %{url: source_url <> "/repos/acme"})

    updated = Hex.Repo.update_organizations(repos)

    # build_url appends /repos/acme, so the stored URL must be the bare source URL
    assert updated["hexpm:acme"].url == source_url
    assert updated["hexpm:acme"].repo_organization == "acme"
  end

  test "update_organizations/1 keeps a custom org URL without repo_organization" do
    repos = Hex.State.fetch!(:repos)
    repos = Map.put(repos, "hexpm:acme", %{url: "http://custom.example/mirror"})

    updated = Hex.Repo.update_organizations(repos)

    assert updated["hexpm:acme"].url == "http://custom.example/mirror"
    refute Map.has_key?(updated["hexpm:acme"], :repo_organization)
  end

  test "clean_organizations/1 round-trips derived and custom org URLs" do
    repos = Hex.State.fetch!(:repos)
    source_url = repos["hexpm"].url

    repos =
      repos
      |> Map.put("hexpm:derived", %{url: source_url <> "/repos/derived"})
      |> Map.put("hexpm:custom", %{url: "http://custom.example/mirror"})

    cleaned = repos |> Hex.Repo.update_organizations() |> Hex.Repo.clean_organizations()

    # Derived URL is rebuilt on read, so it is not persisted
    refute Map.has_key?(cleaned["hexpm:derived"], :url)
    refute Map.has_key?(cleaned["hexpm:derived"], :repo_organization)

    # Custom URL is persisted verbatim
    assert cleaned["hexpm:custom"].url == "http://custom.example/mirror"
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
    test "organization repo inherits oauth_exchange from parent" do
      auth =
        HexTest.Hexpm.new_user(
          "org_oauth_user",
          "org_oauth@example.com",
          "password",
          "org_oauth_key"
        )

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, auth[:key])
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm:testorg", "foo", "")

      repos_after = Hex.State.fetch!(:repos)
      token_data = repos_after["hexpm:testorg"].oauth_token
      assert is_binary(token_data[:access_token])
    end

    test "organization repo skips oauth_exchange when disabled on parent" do
      auth =
        HexTest.Hexpm.new_user(
          "org_no_oauth_user",
          "org_no_oauth@example.com",
          "password",
          "org_no_oauth_key"
        )

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, auth[:key])
      repos = put_in(repos["hexpm"], Map.put(repos["hexpm"], :oauth_exchange, false))
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm:testorg", "foo", "")

      repos_after = Hex.State.fetch!(:repos)
      org_repo = Map.get(repos_after, "hexpm:testorg")
      assert org_repo == nil or Map.get(org_repo, :oauth_token) == nil
    end

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
      assert is_binary(token_data[:access_token])
      assert is_integer(token_data[:expires_at])
      first_token = token_data[:access_token]

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after_2 = Hex.State.fetch!(:repos)
      reused_token = repos_after_2["hexpm"].oauth_token[:access_token]
      assert reused_token == first_token
    end

    test "exchanges for new token when cached token expires" do
      auth = HexTest.Hexpm.new_user("expiry_user", "expiry@example.com", "password", "expiry_key")
      api_key = auth[:key]

      expired_token_data = %{
        access_token: "expired_token",
        expires_at: System.system_time(:second) - 100
      }

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      repos = put_in(repos["hexpm"], Map.put(repos["hexpm"], :oauth_token, expired_token_data))
      Hex.Config.update_repos(repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      new_token = repos_after["hexpm"].oauth_token[:access_token]
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
      token1 = repos_after1["hexpm"].oauth_token[:access_token]

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key2)
      repos = put_in(repos["hexpm"], Map.delete(repos["hexpm"], :oauth_token))
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after2 = Hex.State.fetch!(:repos)
      token2 = repos_after2["hexpm"].oauth_token[:access_token]

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
        access_token: "almost_expired",
        expires_at: System.system_time(:second) + 30
      }

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      repos = put_in(repos["hexpm"], Map.put(repos["hexpm"], :oauth_token, almost_expired_token))
      Hex.Config.update_repos(repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      new_token = repos_after["hexpm"].oauth_token[:access_token]
      assert new_token != "almost_expired"
    end
  end

  describe "OAuth token fallback (device flow)" do
    test "uses OAuth token when no API key is configured" do
      future_time = System.system_time(:second) + 3600

      token_data = %{
        access_token: "device_flow_token",
        refresh_token: "device_refresh",
        expires_at: future_time
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
        access_token: "device_flow_token",
        refresh_token: "device_refresh",
        expires_at: future_time
      }

      Hex.OAuth.store_token(token_data)

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, api_key)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, _}} = Hex.Repo.get_package("hexpm", "postgrex", "")

      repos_after = Hex.State.fetch!(:repos)
      assert repos_after["hexpm"].oauth_token != nil
      assert repos_after["hexpm"].oauth_token[:access_token] != "device_flow_token"
    end

    test "raises error when configured API key exchange fails (no fallback to device flow)" do
      future_time = System.system_time(:second) + 3600

      token_data = %{
        access_token: "device_flow_token",
        refresh_token: "device_refresh",
        expires_at: future_time
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
