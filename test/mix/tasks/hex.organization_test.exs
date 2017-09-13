defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.Case
  @moduletag :integration

  test "auth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("orgauth", "orgauth@mail.com", "password", "orgauth")
      Hexpm.new_repo("myorgauth", auth)
      Mix.Tasks.Hex.update_key(auth[:encrypted_key])

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Organization.run(["auth", "myorgauth"])

      myorg = Hex.Repo.get_repo("hexpm:myorgauth")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorgauth"
      assert is_binary(myorg.auth_key)
    end
  end

  test "auth --key" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("orgauthkey", "orgauthkey@mail.com", "password", "orgauthkey")
      Hexpm.new_repo("myorgauthkey", auth)
      {:ok, {201, body, _}} = Hex.API.Key.new("orgauthkey", [%{"domain" => "repository", "resource" => "myorgauthkey"}], auth)

      Mix.Tasks.Hex.Organization.run(["auth", "myorgauthkey", "--key", body["secret"]])

      myorg = Hex.Repo.get_repo("hexpm:myorgauthkey")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorgauthkey"
      assert myorg.auth_key == body["secret"]
    end
  end

  test "auth --key with invalid key" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      assert_raise Mix.Error, "Failed to authenticate against repository with given key", fn ->
        Mix.Tasks.Hex.Organization.run(["auth", "myorg", "--key", "mykey"])
      end
    end
  end

  test "deauth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("orgdeauth", "orgdeauth@mail.com", "password", "orgdeauth")
      Hexpm.new_repo("myorgdeauth", auth)
      Mix.Tasks.Hex.update_key(auth[:encrypted_key])

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Organization.run(["auth", "myorgdeauth"])

      Mix.Tasks.Hex.Organization.run(["deauth", "myorgdeauth"])
      refute Hex.Config.read()[:"$repos"]["hexpm:myorgdeauth"]
    end
  end

  test "key" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("orgkey", "orgkey@mail.com", "password", "orgkey")
      Hexpm.new_repo("myorgkey", auth)
      Mix.Tasks.Hex.update_key(auth[:encrypted_key])

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Organization.run(["key", "myorgkey"])

      assert_received {:mix_shell, :info, [key]}
      assert is_binary(key)
    end
  end
end
