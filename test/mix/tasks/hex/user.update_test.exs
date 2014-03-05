defmodule Mix.Tasks.Hex.User.UpdateTest do
  use HexTest.Case
  @moduletag :integration

  test "update" do
    { :ok, _ } = HexWeb.User.create("update_user", "old@mail.com", "hunter42")

    send self, { :mix_shell_input, :prompt, "new@mail.com" }
    send self, { :mix_shell_input, :prompt, "" }
    Mix.Tasks.Hex.User.Update.run(["-u", "update_user", "-p", "hunter42"])

    assert_received { :mix_shell, :info, ["Updating user options for update_user was successful!"] }
    assert HexWeb.User.get("update_user").email == "new@mail.com"
  end
end
