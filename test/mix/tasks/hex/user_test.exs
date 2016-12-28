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

    assert {200, body, _} = Hex.API.User.get("eric")
    assert body["username"] == "eric"
    # TODO: re-enable after grace period
    #       or test using a different authenticated endpoint
    # assert body["email"] == "mail@mail.com"
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

  test "list keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth = HexWeb.new_user("list_keys", "list_keys@mail.com", "password", "list_keys")
      Hex.Config.update(auth)

      assert {200, [%{"name" => "list_keys"}], _} = Hex.API.Key.get(auth)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--list"])
      assert_received {:mix_shell, :info, ["list_keys" <> _]}
    end
  end

  test "remove key" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth_a = HexWeb.new_user("remove_key", "remove_key@mail.com", "password", "remove_key_a")
      auth_b = HexWeb.new_key("remove_key", "password", "remove_key_b")
      Hex.Config.update(auth_a)

      assert {200, _, _} = Hex.API.Key.get(auth_a)
      assert {200, _, _} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--remove", "remove_key_b"])
      assert_received {:mix_shell, :info, ["Removing key remove_key_b..."]}

      assert {200, _, _} = Hex.API.Key.get(auth_a)
      assert {401, _, _} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--remove", "remove_key_a"])
      assert_received {:mix_shell, :info, ["Removing key remove_key_a..."]}
      assert_received {:mix_shell, :info, ["User `remove_key` removed from the local machine. To authenticate again, run `mix hex.user auth` or create a new user with `mix hex.user register`"]}

      assert {401, _, _} = Hex.API.Key.get(auth_a)

      config = Hex.Config.read
      refute config[:username]
      refute config[:key]
      refute config[:encrypted_key]
    end
  end

  test "remove all keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth_a = HexWeb.new_user("remove_all_keys", "remove_all_keys@mail.com", "password", "remove_all_keys_a")
      auth_b = HexWeb.new_key("remove_all_keys", "password", "remove_all_keys_b")
      Hex.Config.update(auth_a)

      assert {200, _, _} = Hex.API.Key.get(auth_a)
      assert {200, _, _} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--remove-all"])
      assert_received {:mix_shell, :info, ["Removing all keys..."]}
      assert_received {:mix_shell, :info, ["User `remove_all_keys` removed from the local machine. To authenticate again, run `mix hex.user auth` or create a new user with `mix hex.user register`"]}

      assert {401, _, _} = Hex.API.Key.get(auth_a)
      assert {401, _, _} = Hex.API.Key.get(auth_b)

      config = Hex.Config.read
      refute config[:username]
      refute config[:key]
      refute config[:encrypted_key]
    end
  end
end
