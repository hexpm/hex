defmodule Mix.Tasks.Hex.UserTest do
  use HexTest.Case
  @moduletag :integration

  import ExUnit.CaptureIO

  test "register" do
    send self, {:mix_shell_input, :prompt, "eric"}
    send self, {:mix_shell_input, :prompt, "mail@mail.com"}
    send self, {:mix_shell_input, :yes?, false}

    assert_raise Mix.Error, "Entered passwords do not match", fn ->
      capture_io "hunter42\nhunter43\n", fn ->
        Mix.Tasks.Hex.User.run(["register", "--no-clean-pass"])
      end
    end

    send self, {:mix_shell_input, :prompt, "eric"}
    send self, {:mix_shell_input, :prompt, "mail@mail.com"}
    send self, {:mix_shell_input, :yes?, false}

    capture_io "hunter42\nhunter42\n", fn ->
      Mix.Tasks.Hex.User.run(["register", "--no-clean-pass"])
    end

    assert HexWeb.User.get(username: "eric").email == "mail@mail.com"
  end

  test "update" do
    {:ok, _} = HexWeb.User.create("update_user", "old@mail.com", "hunter42")

    send self, {:mix_shell_input, :prompt, "new@mail.com"}
    send self, {:mix_shell_input, :prompt, ""}
    send self, {:mix_shell_input, :yes?, false}

    capture_io "\n\n", fn ->
      Mix.Tasks.Hex.User.run(["update", "-u", "update_user", "-p", "hunter42", "--no-clean-pass"])
    end

    assert HexWeb.User.get(username: "update_user").email == "new@mail.com"
  end

  test "update config" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      send self, {:mix_shell_input, :prompt, "config"}
      send self, {:mix_shell_input, :prompt, "config@mail.com"}
      send self, {:mix_shell_input, :yes?, true}

    capture_io "hunter42\nhunter42\n", fn ->
      Mix.Tasks.Hex.User.run(["register", "--no-clean-pass"])
    end

      assert Hex.Util.read_config[:username] == "config"
      assert is_binary(Hex.Util.read_config[:key])
    end
  end
end
