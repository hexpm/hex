defmodule Mix.Tasks.Hex.KeyTest do
  use HexTest.Case
  @moduletag :integration

  test "list keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth = HexWeb.new_user("list_keys", "list_keys@mail.com", "password", "list_keys")
      Hex.Config.update(auth)

      assert {200, [%{"name" => "list_keys"}], _} = Hex.API.Key.get(auth)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Key.run(["list"])
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
      Mix.Tasks.Hex.Key.run(["remove", "remove_key_b"])
      assert_received {:mix_shell, :info, ["Removing key remove_key_b..."]}

      assert {200, _, _} = Hex.API.Key.get(auth_a)
      assert {401, _, _} = Hex.API.Key.get(auth_b)

      send self(), {:mix_shell_input, :prompt, "password"}
      Mix.Tasks.Hex.Key.run(["remove", "remove_key_a"])
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
      Mix.Tasks.Hex.Key.run(["remove", "--all"])
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
