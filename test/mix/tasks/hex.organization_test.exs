defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.Case
  @moduletag :integration

  test "auth" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())
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
      Hex.State.put(:home, File.cwd!())

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
      Hex.State.put(:home, File.cwd!())
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

      config = Enum.into(Hex.Config.read(), %{})
      repo = config[:"$repos"]["hexpm:myorgauthkey"]
      assert repo
      assert repo[:auth_key]
      refute repo[:public_key]
      refute repo[:url]
    end)
  end

  test "auth --key with invalid key" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())
      message = "Failed to authenticate against organization repository with given key"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Hex.Organization.run(["auth", "myorg", "--key", "mykey"])
      end
    end)
  end

  test "deauth" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())
      auth = Hexpm.new_user("orgdeauth", "orgdeauth@mail.com", "password", "orgdeauth")
      Hexpm.new_repo("myorgdeauth", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorgdeauth"])

      Mix.Tasks.Hex.Organization.run(["deauth", "myorgdeauth"])
      refute Hex.Config.read()[:"$repos"]["hexpm:myorgdeauth"]
    end)
  end

  test "key --generate" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth = Hexpm.new_user("orgkeygen", "orgkeygen@mail.com", "password", "orgkeygen")
      Hexpm.new_repo("orgkeygen", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeygen", "generate", "--permission", "api:read"]
      Mix.Tasks.Hex.Organization.run(args)

      assert_received {:mix_shell, :info, [key]}
      assert is_binary(key)

      {:ok, hostname} = :inet.gethostname()
      assert {:ok, {200, body, _}} = Hex.API.Key.Organization.get("orgkeygen", key: key)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, body, _}} = Hex.API.Key.Organization.get("orgkeygen", auth)
      assert List.to_string(hostname) in Enum.map(body, & &1["name"])

      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      refute List.to_string(hostname) in Enum.map(body, & &1["name"])
    end)
  end

  test "list keys" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth = Hexpm.new_user("orgkeylist", "orgkeylist@mail.com", "password", "orgkeylist")
      Hexpm.new_repo("orgkeylist", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      send(self(), {:mix_shell_input, :prompt, "password"})
      args = ["key", "orgkeylist", "generate", "--key-name", "orgkeylist"]
      Mix.Tasks.Hex.Organization.run(args)

      assert {:ok, {200, [%{"name" => "orgkeylist"}], _}} =
               Hex.API.Key.Organization.get("orgkeylist", auth)

      Mix.Tasks.Hex.Organization.run(["key", "orgkeylist", "list"])
      assert_received {:mix_shell, :info, ["orgkeylist" <> _]}
    end)
  end

  test "revoke key" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth = Hexpm.new_user("orgkeyrevoke", "orgkeyrevoke@mail.com", "password", "orgkeyrevoke1")
      Hexpm.new_repo("orgkeyrevoke", auth)
      Hexpm.new_organization_key("orgkeyrevoke", "orgkeyrevoke2", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      assert {:ok, {200, [%{"name" => "orgkeyrevoke2"}], _}} =
               Hex.API.Key.Organization.get("orgkeyrevoke", auth)

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevoke", "revoke", "orgkeyrevoke2"])
      assert_received {:mix_shell, :info, ["Revoking key orgkeyrevoke2..."]}

      assert {:ok, {200, [], _}} = Hex.API.Key.Organization.get("orgkeyrevoke", auth)
    end)
  end

  test "revoke all keys" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())
        Hex.State.put(:home, File.cwd!())

        auth = Hexpm.new_user("orgkeyrevokeall", "orgkeyrevokeall@mail.com", "password", "orgkeyrevokeall1")
        Hexpm.new_repo("orgkeyrevokeall", auth)
        Hexpm.new_organization_key("orgkeyrevokeall", "orgkeyrevokeall2", auth)
        Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

        assert {:ok, {200, [%{"name" => "orgkeyrevokeall2"}], _}} =
                 Hex.API.Key.Organization.get("orgkeyrevokeall", auth)

        send(self(), {:mix_shell_input, :prompt, "password"})
        Mix.Tasks.Hex.Organization.run(["key", "orgkeyrevokeall", "revoke", "--all"])
        assert_received {:mix_shell, :info, ["Revoking all keys..."]}

        assert {:ok, {200, [], _}} = Hex.API.Key.Organization.get("orgkeyrevokeall", auth)
    end)
  end
end
