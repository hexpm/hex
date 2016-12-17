defmodule Mix.Tasks.Hex.RetireTest do
  use HexTest.Case
  @moduletag :integration

  test "retire and unretire package" do
    auth = HexWeb.new_user("retire_user", "retire_user@mail.com", "passpass", "key")
    HexWeb.new_package("retire_package", "0.0.1", [], %{}, auth)

    Hex.State.put(:home, tmp_path())
    Hex.Config.update(auth)

    send self(), {:mix_shell_input, :prompt, "passpass"}
    Mix.Tasks.Hex.Retire.run(["retire_package", "0.0.1", "renamed", "--message", "message"])

    assert {200, %{"retirement" => %{"message" => "message", "reason" => "renamed"}}, _} =
           Hex.API.Release.get("retire_package", "0.0.1")

    send self(), {:mix_shell_input, :prompt, "passpass"}
    Mix.Tasks.Hex.Retire.run(["retire_package", "0.0.1", "--unretire"])

    assert {200, %{"retirement" => nil}, _} =
           Hex.API.Release.get("retire_package", "0.0.1")
  end
end
