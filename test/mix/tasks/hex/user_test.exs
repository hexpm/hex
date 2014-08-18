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

  test "auth" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      send self, {:mix_shell_input, :prompt, "user"}

      capture_io "hunter42\n\n", fn ->
        Mix.Tasks.Hex.User.run(["auth", "--no-clean-pass"])
      end

      {:ok, name} = :inet.gethostname()
      name = List.to_string(name)
      user = HexWeb.User.get(username: "user")
      key = HexWeb.API.Key.get(name, user)

      assert Hex.Util.read_config[:username] == "user"
      assert Hex.Util.read_config[:key] == key.secret
    end
  end

  test "update" do
    {:ok, _} = HexWeb.User.create("update_user", "old@mail.com", "hunter42")

    send self, {:mix_shell_input, :prompt, "update_user"}

    send self, {:mix_shell_input, :prompt, "new@mail.com"}
    send self, {:mix_shell_input, :yes?, false}

    capture_io "hunter42\n\n", fn ->
      Mix.Tasks.Hex.User.run(["update", "--no-clean-pass"])
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

  test "whoami" do
    in_tmp fn ->
      Hex.home(System.cwd!)
      Hex.Util.update_config([username: "ausername"])

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, ["ausername"]}
    end
  end
end
