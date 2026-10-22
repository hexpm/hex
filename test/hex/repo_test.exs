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

  test "custom repository uses netrc instead of the global OAuth token" do
    on_exit(fn -> System.delete_env("NETRC") end)

    in_tmp(fn ->
      Hex.State.put(:config_home, File.cwd!())

      File.write!(".netrc", """
      machine localhost
        login repo-user
        password repo-password
      """)

      System.put_env("NETRC", Path.join(File.cwd!(), ".netrc"))

      Hex.OAuth.store_token(%{
        access_token: "hexpm-user-token",
        refresh_token: "hexpm-refresh-token",
        expires_at: System.system_time(:second) + 3600
      })

      bypass = Bypass.open()

      Bypass.expect_once(bypass, "GET", "/tarballs/example-1.0.0.tar", fn conn ->
        assert Plug.Conn.get_req_header(conn, "authorization") == [
                 "Basic #{Base.encode64("repo-user:repo-password")}"
               ]

        Plug.Conn.resp(conn, 200, "tarball")
      end)

      Mix.Tasks.Hex.Repo.run([
        "add",
        "custom",
        "http://localhost:#{bypass.port}"
      ])

      assert {:ok, {200, _, "tarball"}} =
               Hex.Repo.get_tarball("custom", "example", "1.0.0")
    end)
  end

  test "custom Hex.pm organization URL uses the global OAuth token" do
    Hex.OAuth.store_token(%{
      access_token: "hexpm-user-token",
      refresh_token: "hexpm-refresh-token",
      expires_at: System.system_time(:second) + 3600
    })

    bypass = Bypass.open()
    repos = Hex.State.fetch!(:repos)

    repos =
      repos
      |> Map.put("hexpm:acme", %{url: "http://localhost:#{bypass.port}"})
      |> Hex.Repo.update_organizations()

    Hex.State.put(:repos, repos)

    Bypass.expect_once(bypass, "GET", "/tarballs/example-1.0.0.tar", fn conn ->
      assert Plug.Conn.get_req_header(conn, "authorization") == ["Bearer hexpm-user-token"]
      Plug.Conn.resp(conn, 200, "tarball")
    end)

    assert {:ok, {200, _, "tarball"}} =
             Hex.Repo.get_tarball("hexpm:acme", "example", "1.0.0")
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

  describe "Hex.pm organization repository authentication" do
    setup do
      Hex.Server.reset()
      bypass = Bypass.open()
      test_pid = self()

      Bypass.expect(bypass, fn conn ->
        authorization = Plug.Conn.get_req_header(conn, "authorization")
        send(test_pid, {:request, conn.request_path, authorization})
        Plug.Conn.resp(conn, 200, "tarball")
      end)

      %{bypass: bypass}
    end

    test "uses the token from mix hex.organization auth --key over the user's session", %{
      bypass: bypass
    } do
      store_user_token()
      put_organization_repo(bypass, %{oauth_token: token("org-token", 3600)})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", ["Bearer org-token"]}
    end

    test "uses the user's session when the organization's token has expired", %{bypass: bypass} do
      store_user_token()
      put_organization_repo(bypass, %{oauth_token: token("org-token", -100)})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, _path, ["Bearer hexpm-user-token"]}
      refute Case.shell_output() =~ "has expired"
    end

    test "uses the user's session when the server rejects the organization's token", %{
      bypass: bypass
    } do
      test_pid = self()

      Bypass.expect(bypass, fn conn ->
        authorization = Plug.Conn.get_req_header(conn, "authorization")
        send(test_pid, {:request, conn.request_path, authorization})

        case authorization do
          ["Bearer org-token"] ->
            conn
            |> Plug.Conn.put_resp_header(
              "www-authenticate",
              ~s(Bearer realm="hex", error="invalid_token")
            )
            |> Plug.Conn.resp(401, "")

          _other ->
            Plug.Conn.resp(conn, 200, "tarball")
        end
      end)

      store_user_token()
      put_organization_repo(bypass, %{oauth_token: token("org-token", 3600)})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", ["Bearer org-token"]}

      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar",
                       ["Bearer hexpm-user-token"]}
    end

    test "doesn't authenticate with a stored key", %{bypass: bypass} do
      store_user_token()
      put_organization_repo(bypass, %{auth_key: "stored-key"})

      # Exchanging the key with the test server would fail and raise
      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, _path, ["Bearer hexpm-user-token"]}
      refute Case.shell_output() =~ "Hex doesn't use the key stored"
    end

    test "warns when only a stored key is configured", %{bypass: bypass} do
      put_organization_repo(bypass, %{auth_key: "stored-key"})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, _path, []}

      output = Case.shell_output()
      assert output =~ "Hex doesn't use the key stored for the acme repository"
      assert output =~ "mix hex.organization deauth acme"
      assert output =~ "mix hex.user auth"
      assert output =~ "mix hex.organization auth acme --key KEY"
    end

    test "names the stored key, not its expired token, for a Hex 2.5 config", %{bypass: bypass} do
      put_organization_repo(bypass, %{
        auth_key: "stored-key",
        auth_key_owner: :organization,
        oauth_token: token("token-from-stored-key", -100)
      })

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, _path, []}

      output = Case.shell_output()
      assert output =~ "Hex doesn't use the key stored for the acme repository"
      refute output =~ "has expired"
    end

    test "doesn't authenticate with HEX_REPOS_KEY", %{bypass: bypass} do
      Hex.State.put(:repos_key, "repos-key")
      put_organization_repo(bypass, %{})
      assert Hex.Repo.get_repo("hexpm:acme").auth_key == "repos-key"

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", []}

      output = Case.shell_output()
      assert output =~ "Hex doesn't use HEX_REPOS_KEY for organization repositories"
      assert output =~ "mix hex.organization auth acme --key KEY"
    end

    test "doesn't authenticate with the hexpm repository's key", %{bypass: bypass} do
      Hex.State.update!(:repos, &put_in(&1["hexpm"].auth_key, "hexpm-key"))
      put_organization_repo(bypass, %{})
      assert Hex.Repo.get_repo("hexpm:acme").auth_key == "hexpm-key"

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", []}

      output = Case.shell_output()
      assert output =~ "Hex doesn't use the hexpm repository's key for organization repositories"
      refute output =~ "mix hex.organization deauth acme"
    end

    test "sends the user's session to the hexpm repository's URL under an untrusted mirror", %{
      bypass: bypass
    } do
      mirror = Bypass.open()
      Hex.State.put(:mirror_url, "http://localhost:#{mirror.port}")
      store_user_token()
      put_organization_repo(bypass, %{})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")

      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar",
                       ["Bearer hexpm-user-token"]}
    end

    test "doesn't authenticate an organization repository when the hexpm URL is the HEX_MIRROR URL",
         %{bypass: bypass} do
      Hex.State.put(:mirror_url, "http://localhost:#{bypass.port}")
      store_user_token()
      put_organization_repo(bypass, %{oauth_token: token("org-token", 3600)})
      assert Hex.Repo.get_repo("hexpm:acme").trusted == false

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", []}
    end

    test "warns about HEX_REPOS_KEY for an organization loaded when Hex starts", %{
      bypass: bypass
    } do
      Hex.State.put(:repos_key, "repos-key")
      Hex.State.update!(:repos, &put_in(&1["hexpm"].url, "http://localhost:#{bypass.port}"))
      repos = Map.put(Hex.State.fetch!(:repos), "hexpm:acme", %{})

      # Hex.State.init loads the repos in the Hex.State process
      repos = Agent.get(Hex.State, fn _state -> Hex.Repo.update_organizations(repos) end)
      Hex.State.put(:repos, repos)

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", []}
      assert Case.shell_output() =~ "Hex doesn't use HEX_REPOS_KEY for organization repositories"
    end

    test "names HEX_REPOS_KEY when the hexpm repository also has a key", %{bypass: bypass} do
      Hex.State.put(:repos_key, "repos-key")
      Hex.State.update!(:repos, &put_in(&1["hexpm"].auth_key, "hexpm-key"))
      put_organization_repo(bypass, %{})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, _path, []}

      output = Case.shell_output()
      assert output =~ "Hex doesn't use HEX_REPOS_KEY for organization repositories"
      refute output =~ "mix hex.organization deauth acme"
    end

    test "warns when the organization's token has expired and there is no user session", %{
      bypass: bypass
    } do
      put_organization_repo(bypass, %{oauth_token: token("org-token", -100)})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, _path, []}
      assert Case.shell_output() =~ "The token for the acme repository has expired"
    end

    test "fetches an organization repository from the hexpm repository's URL under an untrusted mirror",
         %{bypass: bypass} do
      mirror = Bypass.open()
      Hex.State.put(:mirror_url, "http://localhost:#{mirror.port}")
      put_organization_repo(bypass, %{oauth_token: token("org-token", 3600)})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", ["Bearer org-token"]}
    end

    test "authenticates an organization repository under a trusted mirror", %{bypass: bypass} do
      Hex.State.put(:trusted_mirror_url, "http://localhost:#{bypass.port}")
      put_organization_repo(%{oauth_token: token("org-token", 3600)})

      assert {:ok, {200, _, "tarball"}} = Hex.Repo.get_tarball("hexpm:acme", "foo", "1.0.0")
      assert_received {:request, "/repos/acme/tarballs/foo-1.0.0.tar", ["Bearer org-token"]}
    end
  end

  # Points the hexpm repository at the bypass, so the organization's URL is
  # derived from it the way a hex.pm organization's is.
  defp put_organization_repo(bypass, config) do
    Hex.State.update!(:repos, &put_in(&1["hexpm"].url, "http://localhost:#{bypass.port}"))
    put_organization_repo(config)
  end

  defp put_organization_repo(config) do
    repos =
      Hex.State.fetch!(:repos)
      |> Map.put("hexpm:acme", config)
      |> Hex.Repo.update_organizations()

    Hex.State.put(:repos, repos)
  end

  defp store_user_token do
    Hex.OAuth.store_token(%{
      access_token: "hexpm-user-token",
      refresh_token: "hexpm-refresh-token",
      expires_at: System.system_time(:second) + 3600
    })
  end

  defp token(access_token, expires_in) do
    %{access_token: access_token, expires_at: System.system_time(:second) + expires_in}
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
              trusted: true,
              url: "http://localhost:4043/repo"
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

  test "update_organizations/1 normalizes an org URL saved as the hexpm repository's URL" do
    repos = Hex.State.fetch!(:repos)
    source_url = repos["hexpm"].url
    repos = Map.put(repos, "hexpm:acme", %{url: source_url})

    updated = Hex.Repo.update_organizations(repos)

    assert updated["hexpm:acme"].url == source_url
    assert updated["hexpm:acme"].repo_organization == "acme"
  end

  test "update_organizations/1 derives org URLs from a trusted mirror" do
    Hex.State.put(:trusted_mirror_url, "http://mirror.example")
    repos = Hex.State.fetch!(:repos)
    source_url = repos["hexpm"].url

    repos =
      repos
      |> Map.put("hexpm:acme", %{})
      |> Map.put("hexpm:saved", %{url: source_url})
      |> Map.put("hexpm:legacy", %{url: source_url <> "/repos/legacy"})
      |> Map.put("hexpm:mirror", %{url: "http://mirror.example"})

    updated = Hex.Repo.update_organizations(repos)

    for name <- ["acme", "saved", "legacy", "mirror"] do
      assert updated["hexpm:#{name}"].url == "http://mirror.example"
      assert updated["hexpm:#{name}"].repo_organization == name
      assert updated["hexpm:#{name}"].trusted
    end
  end

  describe "organization repositories loaded from the config" do
    setup do
      env = Map.new(["HEX_HOME", "HEX_MIRROR", "HEX_TRUSTED_MIRROR"], &{&1, System.get_env(&1)})

      on_exit(fn ->
        Enum.each(env, fn
          {name, nil} -> System.delete_env(name)
          {name, value} -> System.put_env(name, value)
        end)
      end)

      in_tmp(fn ->
        Hex.State.put(:config_home, File.cwd!())
        System.put_env("HEX_HOME", File.cwd!())
        System.delete_env("HEX_MIRROR")
        System.delete_env("HEX_TRUSTED_MIRROR")

        Hex.State.fetch!(:repos)
        |> Map.put("hexpm:acme", %{oauth_token: token("org-token", 3600)})
        |> Hex.Config.update_repos()

        %{hexpm_url: Hex.State.fetch!(:repos)["hexpm"].url}
      end)
    end

    test "use the hexpm repository's URL under an untrusted mirror", %{hexpm_url: hexpm_url} do
      System.put_env("HEX_MIRROR", "http://mirror.example")
      Hex.State.refresh()

      assert %{url: ^hexpm_url, repo_organization: "acme", trusted: true} =
               Hex.Repo.get_repo("hexpm:acme")

      Hex.Config.update_repos(Hex.State.fetch!(:repos))

      refute Map.has_key?(Hex.Config.read()[:"$repos"]["hexpm:acme"], :url)

      assert %{url: ^hexpm_url, repo_organization: "acme", trusted: true} =
               Hex.Repo.get_repo("hexpm:acme")
    end

    test "use a trusted mirror" do
      System.put_env("HEX_TRUSTED_MIRROR", "http://mirror.example")
      Hex.State.refresh()

      assert %{url: "http://mirror.example", repo_organization: "acme", trusted: true} =
               Hex.Repo.get_repo("hexpm:acme")

      Hex.Config.update_repos(Hex.State.fetch!(:repos))

      refute Map.has_key?(Hex.Config.read()[:"$repos"]["hexpm:acme"], :url)
    end

    test "use the hexpm repository's URL for a stored untrusted mirror URL", %{
      hexpm_url: hexpm_url
    } do
      Hex.State.fetch!(:repos)
      |> Map.put("hexpm:root", %{url: "http://mirror.example"})
      |> Map.put("hexpm:slash", %{url: "http://mirror.example/"})
      |> Map.put("hexpm:legacy", %{url: "http://mirror.example/repos/legacy"})
      |> Hex.Config.update_repos()

      System.put_env("HEX_MIRROR", "http://mirror.example")
      Hex.State.refresh()

      for name <- ["root", "slash", "legacy"] do
        assert %{url: ^hexpm_url, repo_organization: ^name, trusted: true} =
                 Hex.Repo.get_repo("hexpm:#{name}")
      end
    end

    test "don't trust a custom URL under an untrusted mirror" do
      Hex.State.fetch!(:repos)
      |> Map.put("hexpm:custom", %{url: "http://custom.example/acme"})
      |> Hex.Config.update_repos()

      System.put_env("HEX_MIRROR", "http://mirror.example")
      Hex.State.refresh()

      assert %{url: "http://custom.example/acme", trusted: false} =
               repo = Hex.Repo.get_repo("hexpm:custom")

      refute Map.has_key?(repo, :repo_organization)

      Hex.Config.update_repos(Hex.State.fetch!(:repos))

      assert %{url: "http://custom.example/acme", trusted: false} =
               Hex.Repo.get_repo("hexpm:custom")
    end

    test "keep an organization pinned to repo.hex.pm" do
      Hex.State.refresh()
      Mix.Tasks.Hex.Repo.run(["set", "hexpm", "--url", "http://proxy.example"])
      Mix.Tasks.Hex.Repo.run(["set", "hexpm:acme", "--url", "https://repo.hex.pm/repos/acme"])

      assert %{url: "https://repo.hex.pm", repo_organization: "acme", trusted: true} =
               Hex.Repo.get_repo("hexpm:acme")

      Hex.State.refresh()

      assert %{url: "https://repo.hex.pm", repo_organization: "acme", trusted: true} =
               Hex.Repo.get_repo("hexpm:acme")
    end

    test "follow a change to the hexpm repository's URL" do
      Hex.State.refresh()
      Mix.Tasks.Hex.Repo.run(["set", "hexpm", "--url", "http://new.example"])

      assert %{url: "http://new.example", repo_organization: "acme"} =
               Hex.Repo.get_repo("hexpm:acme")

      refute Map.has_key?(Hex.Config.read()[:"$repos"]["hexpm:acme"], :url)

      Hex.State.refresh()

      assert %{url: "http://new.example", repo_organization: "acme", trusted: true} =
               Hex.Repo.get_repo("hexpm:acme")
    end
  end

  test "update_organizations/1 loads another repository's organization in Hex.State" do
    repos =
      Hex.State.fetch!(:repos)
      |> Map.put("private", %{
        url: "http://private.example",
        public_key: nil,
        auth_key: nil,
        oauth_exchange: false
      })
      |> Map.put("private:acme", %{url: "http://private.example/repos/acme", auth_key: "org-key"})

    # Hex.State.init loads the repos in the Hex.State process, from a config
    # that doesn't store whether a repository is trusted
    updated = Agent.get(Hex.State, fn _state -> Hex.Repo.update_organizations(repos) end)

    assert %{
             url: "http://private.example",
             repo_organization: "acme",
             auth_key: "org-key",
             trusted: true
           } = updated["private:acme"]
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
