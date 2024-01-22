defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.IntegrationCase

  test "auth" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauth", "orgauth@mail.com", "password", "orgauth")
      Hexpm.new_repo("myorgauth", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorgauth"])

      myorg = Hex.Repo.get_repo("hexpm:myorgauth")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorgauth"
      assert is_binary(myorg.auth_key)

      {:ok, hostname} = :inet.gethostname()
      name = "#{hostname}-repository-myorgauth"
      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      assert name in Enum.map(body, & &1["name"])
    end)
  end

  test "auth with --keyname" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_user("orgauthwithkeyname", "orgauthwithkeyname@mail.com", "password", "orgauth")

      Hexpm.new_repo("myorgauthwithkeyname", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})

      Mix.Tasks.Hex.Organization.run([
        "auth",
        "myorgauthwithkeyname",
        "--key-name",
        "orgauthkeyname"
      ])

      myorg = Hex.Repo.get_repo("hexpm:myorgauthwithkeyname")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorgauthwithkeyname"
      assert is_binary(myorg.auth_key)

      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      assert "orgauthkeyname-repository-myorgauthwithkeyname" in Enum.map(body, & &1["name"])
    end)
  end

  test "auth --key" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgauthkey", "orgauthkey@mail.com", "password", "orgauthkey")
      Hexpm.new_repo("myorgauthkey", auth)

      parameters = [%{"domain" => "repository", "resource" => "myorgauthkey"}]
      {:ok, {201, body, _}} = Hex.API.Key.new("orgauthkey", parameters, auth)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgauthkey", "--key", body["secret"]])

      myorg = Hex.Repo.get_repo("hexpm:myorgauthkey")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorgauthkey"
      assert myorg.auth_key == body["secret"]

      repos = Hex.Config.read_repos(Hex.Config.read())
      assert repo = repos["hexpm:myorgauthkey"]
      assert repo[:auth_key]
      assert repo[:trusted]
      assert repo[:url] == "http://localhost:4043/repo/repos/myorgauthkey"

      refute Map.has_key?(Hex.Config.read()[:"$repos"]["hexpm:myorgauthkey"], :trusted)
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
    end)
  end

  test "deauth" do
    in_tmp(fn ->
      set_home_cwd()
      auth = Hexpm.new_user("orgdeauth", "orgdeauth@mail.com", "password", "orgdeauth")
      Hexpm.new_repo("myorgdeauth", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorgdeauth"])

      Mix.Tasks.Hex.Organization.run(["deauth", "myorgdeauth"])
      refute Hex.Config.read_repos(Hex.Config.read())["hexpm:myorgdeauth"]
    end)
  end

  test "key --generate" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_user("orgkeygenuser", "orgkeygenuser@mail.com", "password", "orgkeygenuser")

      Hexpm.new_repo("orgkeygenrepo", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeygenrepo", "generate", "--permission", "api:read"]
      Mix.Tasks.Hex.Organization.run(args)

      assert_received {:mix_shell, :info, [key]}
      assert is_binary(key)

      {:ok, hostname} = :inet.gethostname()
      assert {:ok, {200, body, _}} = Hex.API.Key.Organization.get("orgkeygenrepo", key: key)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, body, _}} = Hex.API.Key.Organization.get("orgkeygenrepo", auth)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      refute List.to_string(hostname) in Enum.map(body, & &1["name"])
    end)
  end

  test "list keys" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_user("orgkeylistuser", "orgkeylistuser@mail.com", "password", "orgkeylistuser")

      Hexpm.new_repo("orgkeylistrepo", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeylistrepo", "generate", "--key-name", "orgkeylistrepo"]
      Mix.Tasks.Hex.Organization.run(args)

      assert {:ok, {200, [%{"name" => "orgkeylistrepo"}], _}} =
               Hex.API.Key.Organization.get("orgkeylistrepo", auth)

      Mix.Tasks.Hex.Organization.run(["key", "orgkeylistrepo", "list"])
      assert_received {:mix_shell, :info, ["orgkeylistrepo" <> _]}
    end)
  end

  test "revoke key" do
    in_tmp(fn ->
      set_home_cwd()

      auth =
        Hexpm.new_user(
          "orgkeyrevokeyuser",
          "orgkeyrevokeyuser@mail.com",
          "password",
          "orgkeyrevokeuser1"
        )

      Hexpm.new_repo("orgkeyrevokerepo", auth)
      Hexpm.new_organization_key("orgkeyrevokerepo", "orgkeyrevokerepo2", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      assert {:ok, {200, [%{"name" => "orgkeyrevokerepo2"}], _}} =
               Hex.API.Key.Organization.get("orgkeyrevokerepo", auth)

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokerepo", "revoke", "orgkeyrevokerepo2"])
      assert_received {:mix_shell, :info, ["Revoking key orgkeyrevokerepo2..."]}

      assert {:ok, {200, [], _}} = Hex.API.Key.Organization.get("orgkeyrevokerepo", auth)
    end)
  end

  test "revoke all keys" do
    in_tmp(fn ->
      set_home_cwd()
      set_home_cwd()

      auth =
        Hexpm.new_user(
          "orgkeyrevokealluser",
          "orgkeyrevokealluser@mail.com",
          "password",
          "orgkeyrevokealluser1"
        )

      Hexpm.new_repo("orgkeyrevokeallrepo", auth)
      Hexpm.new_organization_key("orgkeyrevokeallrepo", "orgkeyrevokeallrepo2", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      assert {:ok, {200, [%{"name" => "orgkeyrevokeallrepo2"}], _}} =
               Hex.API.Key.Organization.get("orgkeyrevokeallrepo", auth)

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokeallrepo", "revoke", "--all"])
      assert_received {:mix_shell, :info, ["Revoking all keys..."]}

      assert {:ok, {200, [], _}} = Hex.API.Key.Organization.get("orgkeyrevokeallrepo", auth)
    end)
  end
end
