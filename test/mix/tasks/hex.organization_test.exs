defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.IntegrationCase

  test "auth requires --key" do
    in_tmp(fn ->
      set_home_cwd()

      message = ~r/Authorizing an organization requires an organization key passed with --key/

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Hex.Organization.run(["auth", "myorg"])
      end

      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorg"]
    end)
  end

  test "auth --key stores a token instead of the key" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_tfa_user("orgauthkey", "orgauthkey@mail.com", "password")
      Hexpm.new_repo("myorgauthkey", auth)

      permissions = [%{"domain" => "repository", "resource" => "myorgauthkey"}]

      {:ok, {201, _, %{"secret" => secret}}} =
        Hex.API.Key.Organization.new("myorgauthkey", "ci", permissions, auth)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgauthkey", "--key", secret])

      refute Case.shell_output() =~ "owned by your user account"

      myorg = Hex.Repo.get_repo("hexpm:myorgauthkey")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo"
      assert myorg.trusted
      assert {:ok, access_token} = Hex.Auth.organization_token(myorg)
      assert access_token != secret

      assert {:ok, "Bearer " <> ^access_token, _auth_context} =
               Hex.Repo.resolve_auth("hexpm:myorgauthkey")

      stored = Hex.Config.read()[:"$repos"]["hexpm:myorgauthkey"]
      assert stored.oauth_token.access_token == access_token
      refute Map.has_key?(stored, :auth_key)
      refute Map.has_key?(stored, :trusted)
    end)
  end

  test "auth --key replaces a key stored by an earlier Hex version" do
    in_tmp(fn ->
      set_home_cwd()

      Hex.State.fetch!(:repos)
      |> Map.put("hexpm:myorgreplace", %{auth_key: "stored-key", auth_key_owner: :user})
      |> Hex.Config.update_repos()

      stub_key_exchange("organization")
      Mix.Tasks.Hex.Organization.run(["auth", "myorgreplace", "--key", "orgkeysecret"])

      stored = Hex.Config.read()[:"$repos"]["hexpm:myorgreplace"]
      assert stored.oauth_token.access_token == "org-token"
      refute Map.has_key?(stored, :auth_key)
      refute Map.has_key?(stored, :auth_key_owner)
    end)
  end

  test "auth --key with a user-owned key warns" do
    in_tmp(fn ->
      set_home_cwd()
      stub_key_exchange("user")

      Mix.Tasks.Hex.Organization.run(["auth", "myuserownedkey", "--key", "userkeysecret"])

      output = Case.shell_output()
      assert output =~ "The given key is owned by your user account, not the organization"
      assert output =~ "hex.pm will stop accepting user keys for organization repositories"
      assert output =~ "mix hex.organization key myuserownedkey generate"

      stored = Hex.Config.read()[:"$repos"]["hexpm:myuserownedkey"]
      assert stored.oauth_token.access_token == "org-token"
    end)
  end

  test "auth --key against a server without key owner information doesn't warn" do
    in_tmp(fn ->
      set_home_cwd()
      stub_key_exchange(nil)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgnoowner", "--key", "noownersecret"])

      refute Case.shell_output() =~ "owned by your user account"

      stored = Hex.Config.read()[:"$repos"]["hexpm:myorgnoowner"]
      assert stored.oauth_token.access_token == "org-token"
    end)
  end

  test "auth --key with invalid key" do
    in_tmp(fn ->
      set_home_cwd()

      assert catch_throw(Mix.Tasks.Hex.Organization.run(["auth", "myorg", "--key", "mykey"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :error,
                       [
                         "Failed to authenticate against organization repository with given key because of: invalid API key"
                       ]}

      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorg"]
    end)
  end

  test "auth --key when the key can't be exchanged" do
    in_tmp(fn ->
      set_home_cwd()
      bypass = Bypass.open()
      Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

      Bypass.expect(bypass, fn conn ->
        case conn.request_path do
          "/api/auth" -> erlang_resp(conn, 200, %{})
          "/api/oauth/token" -> erlang_resp(conn, 401, %{"error" => "invalid_client"})
        end
      end)

      assert catch_throw(
               Mix.Tasks.Hex.Organization.run(["auth", "myorgnoexchange", "--key", "key"])
             ) == {:exit_code, 1}

      assert_received {:mix_shell, :error,
                       ["Failed to exchange the key for a token: invalid_client"]}

      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgnoexchange"]
    end)
  end

  test "deauth" do
    in_tmp(fn ->
      set_home_cwd()
      stub_key_exchange("organization")

      Mix.Tasks.Hex.Organization.run(["auth", "myorgdeauth", "--key", "orgkeysecret"])
      assert Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgdeauth"]

      Mix.Tasks.Hex.Organization.run(["deauth", "myorgdeauth"])
      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgdeauth"]
    end)
  end

  test "key --generate" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_tfa_user("orgkeygenuser", "orgkeygenuser@mail.com", "password")
      Hexpm.sign_in(auth)

      Hexpm.new_repo("orgkeygenrepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeygenrepo", "generate", "--permission", "api:read"]
      Mix.Tasks.Hex.Organization.run(args)

      assert_received {:mix_shell, :info, [key]}
      assert is_binary(key)

      {:ok, hostname} = :inet.gethostname()
      assert {:ok, {200, _, body}} = Hex.API.Key.Organization.get("orgkeygenrepo", key: key)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, _, body}} = Hex.API.Key.Organization.get("orgkeygenrepo", auth)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, _, body}} = Hex.API.Key.get(auth)
      refute List.to_string(hostname) in Enum.map(body, & &1["name"])
    end)
  end

  test "list keys" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_tfa_user("orgkeylistuser", "orgkeylistuser@mail.com", "password")
      Hexpm.sign_in(auth)

      Hexpm.new_repo("orgkeylistrepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeylistrepo", "generate", "--key-name", "orgkeylistrepo"]
      Mix.Tasks.Hex.Organization.run(args)

      assert {:ok, {200, _, [%{"name" => "orgkeylistrepo"}]}} =
               Hex.API.Key.Organization.get("orgkeylistrepo", auth)

      Mix.Tasks.Hex.Organization.run(["key", "orgkeylistrepo", "list"])
      assert_received {:mix_shell, :info, ["orgkeylistrepo" <> _]}
    end)
  end

  test "revoke key" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_tfa_user("orgkeyrevokeyuser", "orgkeyrevokeyuser@mail.com", "password")
      Hexpm.sign_in(auth)

      Hexpm.new_repo("orgkeyrevokerepo", auth)
      Hexpm.new_organization_key("orgkeyrevokerepo", "orgkeyrevokerepo2", auth)

      assert {:ok, {200, _, [%{"name" => "orgkeyrevokerepo2"}]}} =
               Hex.API.Key.Organization.get("orgkeyrevokerepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokerepo", "revoke", "orgkeyrevokerepo2"])
      assert_received {:mix_shell, :info, ["Revoking key orgkeyrevokerepo2..."]}

      assert {:ok, {200, _, []}} = Hex.API.Key.Organization.get("orgkeyrevokerepo", auth)
    end)
  end

  test "revoke all keys" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_tfa_user("orgkeyrevokealluser", "orgkeyrevokealluser@mail.com", "password")

      Hexpm.sign_in(auth)

      Hexpm.new_repo("orgkeyrevokeallrepo", auth)
      Hexpm.new_organization_key("orgkeyrevokeallrepo", "orgkeyrevokeallrepo2", auth)

      assert {:ok, {200, _, [%{"name" => "orgkeyrevokeallrepo2"}]}} =
               Hex.API.Key.Organization.get("orgkeyrevokeallrepo", auth)

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokeallrepo", "revoke", "--all"])
      assert_received {:mix_shell, :info, ["Revoking all keys..."]}

      assert {:ok, {200, _, []}} = Hex.API.Key.Organization.get("orgkeyrevokeallrepo", auth)
    end)
  end

  test "escapes timestamps in organization key listings" do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")
    Hex.State.put(:api_key, "test-key")
    body = [%{"name" => "ci", "inserted_at" => "2026-09-13T12:00:00Z\e[2K\rFORGED"}]

    Bypass.expect(bypass, "GET", "/api/orgs/private/keys", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
      |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(body))
    end)

    Mix.Tasks.Hex.Organization.run(["key", "private", "list"])
    output = Case.shell_output()
    assert output =~ "2026-09-13T12:00:00Z\\e[2K\\rFORGED"
    refute output =~ "\e"
    refute output =~ "\r"
  end

  # Answers the key check with a key owned by owner_type, nil for a server
  # that doesn't say who owns the key, and the exchange with a token.
  defp stub_key_exchange(owner_type) do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    auth_body =
      if owner_type,
        do: %{"key" => %{"name" => "ci", "owner" => %{"type" => owner_type, "name" => "owner"}}},
        else: %{}

    Bypass.expect(bypass, fn conn ->
      case conn.request_path do
        "/api/auth" ->
          erlang_resp(conn, 200, auth_body)

        "/api/oauth/token" ->
          erlang_resp(conn, 200, %{
            "access_token" => "org-token",
            "token_type" => "Bearer",
            "expires_in" => 1800
          })
      end
    end)
  end

  defp erlang_resp(conn, status, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/vnd.hex+erlang")
    |> Plug.Conn.resp(status, :erlang.term_to_binary(payload))
  end
end
