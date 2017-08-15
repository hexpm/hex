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

    assert {:ok, {200, body, _}} = Hex.API.User.get("eric")
    assert body["username"] == "eric"
  end

  test "auth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      send self(), {:mix_shell_input, :prompt, "user"}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.User.run(["auth"])

      {:ok, name} = :inet.gethostname()
      name = List.to_string(name)

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      auth = Mix.Tasks.Hex.auth_info("hexpm")
      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      assert name in Enum.map(body, &(&1["name"]))
    end
  end

  test "deauth" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Tasks.Hex.update_key("hexpm", "key")
      assert Hex.State.fetch!(:repos)["hexpm"].api_key == "key"

      Mix.Tasks.Hex.User.run(["deauth"])
      refute Hex.State.fetch!(:repos)["hexpm"].api_key
    end
  end

  test "passphrase" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Tasks.Hex.update_key("hexpm", Mix.Tasks.Hex.encrypt_key("hunter42", "qwerty"))
      first_key = Hex.State.fetch!(:repos)["hexpm"].api_key

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      send self(), {:mix_shell_input, :prompt, "hunter43"}
      send self(), {:mix_shell_input, :prompt, "hunter43"}
      Mix.Tasks.Hex.User.run(["passphrase"])

      assert Hex.State.fetch!(:repos)["hexpm"].api_key != first_key

      assert_raise Mix.Error, ~r"^Wrong passphrase", fn ->
        send self(), {:mix_shell_input, :prompt, "wrong"}
        Mix.Tasks.Hex.User.run(["passphrase"])
      end
    end
  end

  test "whoami" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      auth = Hexpm.new_user("whoami", "whoami@mail.com", "password", "whoami")
      Mix.Tasks.Hex.update_key("hexpm", auth[:encrypted_key])

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, ["whoami"]}
    end
  end

  test "list keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth = Hexpm.new_user("list_keys", "list_keys@mail.com", "password", "list_keys")
      Mix.Tasks.Hex.update_key("hexpm", auth[:encrypted_key])

      assert {:ok, {200, [%{"name" => "list_keys"}], _}} = Hex.API.Key.get(auth)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--list"])
      assert_received {:mix_shell, :info, ["list_keys" <> _]}
    end
  end

  test "remove key" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth_a = Hexpm.new_user("remove_key", "remove_key@mail.com", "password", "remove_key_a")
      auth_b = Hexpm.new_key("remove_key", "password", "remove_key_b")
      Mix.Tasks.Hex.update_key("hexpm", auth_a[:encrypted_key])

      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--remove", "remove_key_b"])
      assert_received {:mix_shell, :info, ["Removing key remove_key_b..."]}

      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--remove", "remove_key_a"])
      assert_received {:mix_shell, :info, ["Removing key remove_key_a..."]}
      assert_received {:mix_shell, :info, ["Authentication credentials removed from the local machine." <> _]}

      assert {:ok ,{401, _, _}} = Hex.API.Key.get(auth_a)
    end
  end

  test "remove all keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth_a = Hexpm.new_user("remove_all_keys", "remove_all_keys@mail.com", "password", "remove_all_keys_a")
      auth_b = Hexpm.new_key("remove_all_keys", "password", "remove_all_keys_b")
      Mix.Tasks.Hex.update_key("hexpm", auth_a[:encrypted_key])

      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.User.run(["key", "--remove-all"])
      assert_received {:mix_shell, :info, ["Removing all keys..."]}
      assert_received {:mix_shell, :info, ["Authentication credentials removed from the local machine." <> _]}

      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_b)
    end
  end

  test "reset password" do
    Hexpm.new_user("reset_password", "reset_password@mail.com", "password", "reset_password")

    send self(), {:mix_shell_input, :prompt, "reset_password"}
    Mix.Tasks.Hex.User.run(["reset", "password"])

    assert_received {:mix_shell, :info, ["We’ve sent you an email" <> _]}
  end
end
