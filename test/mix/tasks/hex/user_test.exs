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
      assert HexWeb.API.Key.get(name, user)

      assert Hex.Config.read[:username] == "user"
      assert byte_size(Hex.Config.read[:key]) == 32
    end
  end

  test "deauth" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      Hex.Config.update(username: "johndoe", key: "qwertyuiop",
                        xyz: "other", foo: :bar)
      Mix.Tasks.Hex.User.run(["deauth"])

      assert Dict.take(Hex.Config.read, [:username, :key]) == []
    end
  end

  test "test" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      user = HexWeb.User.get(username: "user")
      {:ok, key} = HexWeb.API.Key.create(user, %{"name" => "computer"})
      Hex.Config.update(username: "user", key: key.user_secret)
      Mix.Tasks.Hex.User.run(["test"])

      assert_received {:mix_shell, :info, ["Successfully authed. Your key works."]}

      Hex.Config.update(username: "user", key: "wrong_key")
      Mix.Tasks.Hex.User.run(["test"])
      assert_received {:mix_shell, :error, ["Failed to auth"]}
    end
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

      assert Hex.Config.read[:username] == "config"
      assert is_binary(Hex.Config.read[:key])
    end
  end

  test "whoami" do
    in_tmp fn ->
      Hex.home(System.cwd!)
      Hex.Config.update([username: "ausername"])

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, ["ausername"]}
    end
  end
end
