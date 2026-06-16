defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.IntegrationCase

  test "auth without --key raises" do
    in_tmp(fn ->
      set_home_cwd()

      assert_raise Mix.Error, ~r/requires an organization key passed with --key/, fn ->
        Mix.Tasks.Hex.Organization.run(["auth", "myorg"])
      end
    end)
  end

  test "auth --key exchanges the key for a short-lived token" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauthkey", "orgauthkey@mail.com", "password", "key")
      Hexpm.new_repo("myorgauthkey", auth)

      parameters = [%{"domain" => "repository", "resource" => "myorgauthkey"}]
      {:ok, {201, _, body}} = Hex.API.Key.new("orgauthkey", parameters, auth)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgauthkey", "--key", body["secret"]])

      myorg = Hex.Repo.get_repo("hexpm:myorgauthkey")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorgauthkey"

      # Only the short-lived token is stored, never the key
      refute myorg[:auth_key]
      assert %{"access_token" => access_token, "expires_at" => expires_at} = myorg.oauth_token
      assert is_binary(access_token)
      assert is_integer(expires_at)

      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repo = repos["hexpm:myorgauthkey"]
      refute repo[:auth_key]
      assert repo[:oauth_token]["access_token"] == access_token
    end)
  end

  test "auth --key with a key that does not grant access to the organization" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauthwrong", "orgauthwrong@mail.com", "password", "key")
      Hexpm.new_repo("orgauthwrong_a", auth)
      Hexpm.new_repo("orgauthwrong_b", auth)

      parameters = [%{"domain" => "repository", "resource" => "orgauthwrong_a"}]
      {:ok, {201, _, body}} = Hex.API.Key.new("orgauthwrong", parameters, auth)

      assert catch_throw(
               Mix.Tasks.Hex.Organization.run([
                 "auth",
                 "orgauthwrong_b",
                 "--key",
                 body["secret"]
               ])
             ) == {:exit_code, 1}

      assert_received {:mix_shell, :error, [error]}
      assert error =~ "does not grant access to the orgauthwrong_b repository"
      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:orgauthwrong_b"]
    end)
  end

  test "auth --key with invalid key" do
    in_tmp(fn ->
      set_home_cwd()

      assert catch_throw(Mix.Tasks.Hex.Organization.run(["auth", "myorg", "--key", "mykey"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :error, [error]}
      assert error =~ "Failed to authenticate against the myorg repository"
    end)
  end

  test "deauth" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgdeauth", "orgdeauth@mail.com", "password", "key")
      Hexpm.new_repo("myorgdeauth", auth)

      parameters = [%{"domain" => "repository", "resource" => "myorgdeauth"}]
      {:ok, {201, _, body}} = Hex.API.Key.new("orgdeauth", parameters, auth)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgdeauth", "--key", body["secret"]])
      assert Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgdeauth"]

      Mix.Tasks.Hex.Organization.run(["deauth", "myorgdeauth"])
      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgdeauth"]
    end)
  end

  test "key --generate" do
    in_tmp(fn ->
      set_home_cwd()

      auth = Hexpm.new_user("orgkeygenuser", "orgkeygenuser@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

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

      auth = Hexpm.new_user("orgkeylistuser", "orgkeylistuser@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

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

      auth = Hexpm.new_user("orgkeyrevokeyuser", "orgkeyrevokeyuser@mail.com", "password", "key")
      Hex.State.put(:api_key, auth[:key])

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
        Hexpm.new_user("orgkeyrevokealluser", "orgkeyrevokealluser@mail.com", "password", "key")

      Hex.State.put(:api_key, auth[:key])

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
end
