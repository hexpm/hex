defmodule Mix.Tasks.Hex.RegisterTest do
  use HexTest.Case
  @moduletag :integration

  test "create" do
    send self, { :mix_shell_input, :prompt, "eric" }
    send self, { :mix_shell_input, :prompt, "mail@mail.com" }
    send self, { :mix_shell_input, :prompt, "hunter42" }
    Mix.Tasks.Hex.User.Register.run([])
    assert_received { :mix_shell, :info, ["Registration of user eric successful!"] }
  end
end
