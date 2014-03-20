defmodule Mix.Tasks.Hex.User.Test do
  use HexTest.Case
  @moduletag :integration

  test "register" do
    send self, { :mix_shell_input, :prompt, "eric" }
    send self, { :mix_shell_input, :prompt, "mail@mail.com" }
    send self, { :mix_shell_input, :prompt, "hunter42" }
    send self, { :mix_shell_input, :prompt, "hunter43" }
    send self, { :mix_shell_input, :yes?, false }

    assert_raise Mix.Error, "Entered passwords do not match", fn ->
      Mix.Tasks.Hex.User.Register.run([])
    end

    send self, { :mix_shell_input, :prompt, "eric" }
    send self, { :mix_shell_input, :prompt, "mail@mail.com" }
    send self, { :mix_shell_input, :prompt, "hunter42" }
    send self, { :mix_shell_input, :prompt, "hunter42" }
    send self, { :mix_shell_input, :yes?, false }
    Mix.Tasks.Hex.User.Register.run([])
    assert_received { :mix_shell, :info, ["Registration of user eric was successful!"] }
  end

  test "update" do
    { :ok, _ } = HexWeb.User.create("update_user", "old@mail.com", "hunter42")

    send self, { :mix_shell_input, :prompt, "new@mail.com" }
    send self, { :mix_shell_input, :prompt, "" }
    send self, { :mix_shell_input, :yes?, false }
    Mix.Tasks.Hex.User.Update.run(["-u", "update_user", "-p", "hunter42"])

    assert_received { :mix_shell, :info, ["Updating user options for update_user was successful!"] }
    assert HexWeb.User.get("update_user").email == "new@mail.com"
  end

  test "update config" do
    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)

      send self, { :mix_shell_input, :prompt, "config" }
      send self, { :mix_shell_input, :prompt, "config@mail.com" }
      send self, { :mix_shell_input, :prompt, "hunter42" }
      send self, { :mix_shell_input, :prompt, "hunter42" }
      send self, { :mix_shell_input, :yes?, true }

      Mix.Tasks.Hex.User.Register.run([])
      assert_received { :mix_shell, :info, ["Registration of user config was successful!"] }

      assert Hex.Mix.read_config[:username] == "config"
      assert is_binary(Hex.Mix.read_config[:key])
    end
  after
    System.delete_env("MIX_HOME")
  end
end
