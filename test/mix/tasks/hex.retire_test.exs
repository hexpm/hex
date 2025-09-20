defmodule Mix.Tasks.Hex.RetireTest do
  use HexTest.IntegrationCase

  test "retire and unretire package" do
    auth = Hexpm.new_user("retire_user", "retire_user@mail.com", "passpass", "key")
    Hexpm.new_package("hexpm", "retire_package", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Retire.run(["retire_package", "0.0.1", "renamed", "--message", "message"])

    assert {:ok, {200, _, %{"retirement" => %{"message" => "message", "reason" => "renamed"}}}} =
             Hex.API.Release.get("hexpm", "retire_package", "0.0.1")

    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Retire.run(["retire_package", "0.0.1", "--unretire"])

    assert {:ok, {200, _, %{"retirement" => nil}}} =
             Hex.API.Release.get("hexpm", "retire_package", "0.0.1")
  end

  test "retire --message flag is required" do
    auth =
      Hexpm.new_user("retire_user_message", "retire_user_message@mail.com", "passpass", "key")

    Hexpm.new_package("hexpm", "retire_package_message", "0.0.1", [], %{}, auth)

    set_home_tmp()
    Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])
    send(self(), {:mix_shell_input, :prompt, "passpass"})

    assert_raise Mix.Error, "Missing required flag --message", fn ->
      Mix.Tasks.Hex.Retire.run(["retire_package", "0.0.1", "renamed"])
    end
  end
end
