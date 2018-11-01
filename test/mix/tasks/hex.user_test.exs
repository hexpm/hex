defmodule Mix.Tasks.Hex.UserTest do
  use HexTest.Case
  @moduletag :integration

  test "register" do
    send(self(), {:mix_shell_input, :prompt, "eric"})
    send(self(), {:mix_shell_input, :prompt, "mail@mail.com"})
    send(self(), {:mix_shell_input, :yes?, false})
    send(self(), {:mix_shell_input, :prompt, "hunter42"})
    send(self(), {:mix_shell_input, :prompt, "hunter43"})

    assert_raise Mix.Error, "Entered passwords do not match", fn ->
      Mix.Tasks.Hex.User.run(["register"])
    end

    send(self(), {:mix_shell_input, :prompt, "eric"})
    send(self(), {:mix_shell_input, :prompt, "mail@mail.com"})
    send(self(), {:mix_shell_input, :yes?, false})
    send(self(), {:mix_shell_input, :prompt, "hunter42"})
    send(self(), {:mix_shell_input, :prompt, "hunter42"})
    send(self(), {:mix_shell_input, :prompt, "hunter43"})
    send(self(), {:mix_shell_input, :prompt, "hunter43"})

    Mix.Tasks.Hex.User.run(["register"])

    assert {:ok, {200, body, _}} = Hex.API.User.get("eric")
    assert body["username"] == "eric"
  end

  test "auth" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      send(self(), {:mix_shell_input, :prompt, "user"})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      Mix.Tasks.Hex.User.run(["auth"])

      {:ok, name} = :inet.gethostname()
      name = List.to_string(name)

      auth = Mix.Tasks.Hex.auth_info(:read)
      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      assert "#{name}-api" in Enum.map(body, & &1["name"])
      assert "#{name}-repositories" in Enum.map(body, & &1["name"])
    end)
  end

  test "auth with --key-name" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      send(self(), {:mix_shell_input, :prompt, "user"})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      Mix.Tasks.Hex.User.run(["auth", "--key-name", "userauthkeyname"])

      auth = Mix.Tasks.Hex.auth_info(:read)
      assert {:ok, {200, body, _}} = Hex.API.Key.get(auth)
      assert "userauthkeyname-api" in Enum.map(body, & &1["name"])
      assert "userauthkeyname-repositories" in Enum.map(body, & &1["name"])
    end)
  end

  test "auth organizations" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth = Hexpm.new_user("userauthorg", "userauthorg@mail.com", "password", "userauthorg")
      Hexpm.new_repo("myuserauthorg", auth)

      send(self(), {:mix_shell_input, :prompt, "userauthorg"})
      send(self(), {:mix_shell_input, :prompt, "password"})
      send(self(), {:mix_shell_input, :prompt, "password"})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.User.run(["auth"])

      assert {:ok, hexpm_repo} = Hex.Repo.fetch_repo("hexpm")
      assert {:ok, neworg_repo} = Hex.Repo.fetch_repo("hexpm:myuserauthorg")
      assert is_binary(hexpm_repo.auth_key)
      assert hexpm_repo.auth_key == neworg_repo.auth_key
    end)
  end

  test "deauth user and organizations" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth = Hexpm.new_user("userdeauth1", "userdeauth1@mail.com", "password", "userdeauth1")
      Hexpm.new_repo("myorguserdeauth1", auth)
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])
      assert Hex.Config.read()[:"$write_key"] == auth[:"$write_key"]

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.Organization.run(["auth", "myorguserdeauth1"])

      Mix.Tasks.Hex.User.run(["deauth"])
      refute Hex.Config.read()[:"$write_key"]
      refute Hex.Config.read()[:"$repos"]["hexpm:myorguserdeauth1"]
    end)
  end

  test "whoami" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())
      auth = Hexpm.new_user("whoami", "whoami@mail.com", "password", "whoami")
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      Mix.Tasks.Hex.User.run(["whoami"])
      assert_received {:mix_shell, :info, ["whoami"]}
    end)
  end

  test "list keys" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth = Hexpm.new_user("list_keys", "list_keys@mail.com", "password", "list_keys")
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])

      assert {:ok, {200, [%{"name" => "list_keys"}], _}} = Hex.API.Key.get(auth)

      Mix.Tasks.Hex.User.run(["key", "list"])
      assert_received {:mix_shell, :info, ["list_keys" <> _]}
    end)
  end

  test "revoke key" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth_a = Hexpm.new_user("revoke_key", "revoke_key@mail.com", "password", "revoke_key_a")
      auth_b = Hexpm.new_key("revoke_key", "password", "revoke_key_b")
      Mix.Tasks.Hex.update_keys(auth_a[:"$write_key"], auth_a[:"$read_key"])

      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_b)

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.User.run(["key", "revoke", "revoke_key_b"])
      assert_received {:mix_shell, :info, ["Revoking key revoke_key_b..."]}

      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_b)

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.User.run(["key", "revoke", "revoke_key_a"])
      assert_received {:mix_shell, :info, ["Revoking key revoke_key_a..."]}

      message =
        "Authentication credentials removed from the local machine. " <>
          "To authenticate again, run `mix hex.user auth` or create a new user with " <>
          "`mix hex.user register`"

      assert_received {:mix_shell, :info, [^message]}

      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_a)
    end)
  end

  test "revoke all keys" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      auth_a =
        Hexpm.new_user(
          "revoke_all_keys",
          "revoke_all_keys@mail.com",
          "password",
          "revoke_all_keys_a"
        )

      auth_b = Hexpm.new_key("revoke_all_keys", "password", "revoke_all_keys_b")
      Mix.Tasks.Hex.update_keys(auth_a[:"$write_key"], auth_a[:"$read_key"])

      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {200, _, _}} = Hex.API.Key.get(auth_b)

      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.User.run(["key", "revoke", "--all"])
      assert_received {:mix_shell, :info, ["Revoking all keys..."]}

      message =
        "Authentication credentials removed from the local machine. " <>
          "To authenticate again, run `mix hex.user auth` or create a new user with " <>
          "`mix hex.user register`"

      assert_received {:mix_shell, :info, [^message]}

      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_a)
      assert {:ok, {401, _, _}} = Hex.API.Key.get(auth_b)
    end)
  end

  test "key generate" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())
      Hexpm.new_user("userkeygenerate", "userkeygenerate@mail.com", "password", "password")
      send(self(), {:mix_shell_input, :prompt, "userkeygenerate"})
      send(self(), {:mix_shell_input, :prompt, "password"})
      Mix.Tasks.Hex.User.run(["key", "generate"])
      assert_received {:mix_shell, :info, ["Generating key..."]}
      assert_received {:mix_shell, :info, [key]}
      assert is_binary(key)
    end)
  end

  test "reset account password" do
    Hexpm.new_user("reset_password", "reset_password@mail.com", "password", "reset_password")

    send(self(), {:mix_shell_input, :prompt, "reset_password"})
    Mix.Tasks.Hex.User.run(["reset_password", "account"])

    assert_received {:mix_shell, :info, ["We’ve sent you an email" <> _]}
  end

  test "reset local password" do
    in_tmp(fn ->
      Hex.State.put(:home, File.cwd!())

      Mix.Tasks.Hex.update_keys(Mix.Tasks.Hex.encrypt_key("hunter42", "qwerty"))
      first_key = Hex.Config.read()[:"$write_key"]
      read_key = Hex.Config.read()[:"$read_key"]

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      Mix.Tasks.Hex.User.run(["reset_password", "local"])

      assert Hex.Config.read()[:"$write_key"] != first_key
      assert Hex.Config.read()[:"$read_key"] == read_key

      send(self(), {:mix_shell_input, :prompt, "wrong"})
      send(self(), {:mix_shell_input, :prompt, "hunter43"})
      send(self(), {:mix_shell_input, :prompt, "hunter44"})
      send(self(), {:mix_shell_input, :prompt, "hunter44"})
      Mix.Tasks.Hex.User.run(["reset_password", "local"])
      assert_received {:mix_shell, :error, ["Wrong password. Try again"]}
    end)
  end
end
