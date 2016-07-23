defmodule Mix.Tasks.Hex.UserTest do
  use HexTest.Case
  @moduletag :integration

  test "register" do
    send self(), {:mix_shell_input, :prompt, "eric"}
    send self(), {:mix_shell_input, :prompt, "mail@mail.com"}
    send self(), {:mix_shell_input, :yes?, false}
    send self(), {:mix_shell_input, :prompt, "hunter42"}
    send self(), {:mix_shell_input, :prompt, "hunter43"}

    assert_raise Mix.Error, "Entered passwords do not match", fn ->
      Mix.Tasks.Hex.User.run(["register"])
    end

    send self(), {:mix_shell_input, :prompt, "eric"}
    send self(), {:mix_shell_input, :prompt, "mail@mail.com"}
    send self(), {:mix_shell_input, :yes?, false}
    send self(), {:mix_shell_input, :prompt, "hunter42"}
    send self(), {:mix_shell_input, :prompt, "hunter42"}

    Mix.Tasks.Hex.User.run(["register"])

    auth = get_auth("eric", "hunter42")
    assert {200, body, _} = Hex.API.User.get("eric", auth)
    assert body["email"] == "mail@mail.com"
  end

  test "auth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      send self(), {:mix_shell_input, :prompt, "user"}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.User.run(["auth"])

      {:ok, name} = :inet.gethostname()
      name = List.to_string(name)
      config = Hex.Config.read

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      auth = Mix.Hex.Utils.auth_info(Hex.Config.read)
      assert {200, body, _} = Hex.API.Key.get(auth)
      assert name in Enum.map(body, &(&1["name"]))

      assert config[:username] == "user"
      assert config[:encrypted_key]
    end
  end

  test "deauth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Hex.Config.update(username: "johndoe", key: "qwertyuiop",
                        encrypted_key: "...",
                        xyz: "other", foo: :bar)
      Mix.Tasks.Hex.User.run(["deauth"])

      assert Keyword.take(Hex.Config.read, [:username, :key, :encrypted_key]) == []
    end
  end

  test "passphrase" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Hex.Config.update(key: "qwertyuiop")
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.User.run(["passphrase"])

      config = Hex.Config.read
      assert config[:encrypted_key]
      refute config[:key]

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      send self(), {:mix_shell_input, :prompt, "hunter43"}
      send self(), {:mix_shell_input, :prompt, "hunter43"}
      Mix.Tasks.Hex.User.run(["passphrase"])

      config = Hex.Config.read
      assert config[:encrypted_key]

      assert_raise Mix.Error, "Wrong passphrase", fn ->
        send self(), {:mix_shell_input, :prompt, "wrong"}
        Mix.Tasks.Hex.User.run(["passphrase"])
      end
    end
  end

  test "test" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      send self(), {:mix_shell_input, :prompt, "user"}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.User.run(["auth"])

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.User.run(["test"])
      assert_received {:mix_shell, :info, ["Successfully authed. Your key works."]}

      send self(), {:mix_shell_input, :prompt, "hunter43"}
      assert_raise(Mix.Error, fn ->
        Mix.Tasks.Hex.User.run(["test"])
      end)

      Mix.Hex.Utils.persist_key("hunter42", "wrongkey")
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.User.run(["test"])
      assert_received {:mix_shell, :error, ["Failed to auth"]}
    end
  end

  test "update config" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      send self(), {:mix_shell_input, :prompt, "config"}
      send self(), {:mix_shell_input, :prompt, "config@mail.com"}
      send self(), {:mix_shell_input, :yes?, true}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      send self(), {:mix_shell_input, :prompt, "hunter42"}

      Mix.Tasks.Hex.User.run(["register"])

      config = Hex.Config.read
      assert config[:username] == "config"
      assert is_binary(config[:encrypted_key])
    end
  end

  test "whoami" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Hex.Config.update([username: "ausername"])

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, ["ausername"]}
    end
  end
end
