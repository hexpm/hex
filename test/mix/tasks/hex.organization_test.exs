defmodule Mix.Tasks.Hex.OrganizationTest do
  use HexTest.Case

  test "auth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("orgauth", "orgauth@mail.com", "password", "orgauth")
      Mix.Tasks.Hex.update_key(auth[:encrypted_key])

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Organization.run(["auth", "myorg"])

      myorg = Hex.Repo.get_repo("hexpm:myorg")
      hexpm = Hex.Repo.get_repo("hexpm")

      assert myorg.public_key == hexpm.public_key
      assert myorg.url == "http://localhost:4043/repo/repos/myorg"
      assert is_binary(myorg.auth_key)
    end
  end

  test "deauth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("orgdeauth", "orgdeauth@mail.com", "password", "orgdeauth")
      Mix.Tasks.Hex.update_key(auth[:encrypted_key])

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Organization.run(["auth", "myorg"])

      Mix.Tasks.Hex.Organization.run(["deauth", "myorg"])
      refute Hex.Config.read()[:"$repos"]["hexpm:myorg"]
    end
  end
end
