defmodule Mix.Tasks.Hex.KeyTest do
  use HexTest.Case
  @moduletag :integration

  test "new key" do
    in_tmp fn ->
      System.put_env("MIX_HOME", System.cwd!)
      Mix.Tasks.Hex.Key.run(["new", "-u", "user", "-p", "hunter42"])

      {:ok, name} = :inet.gethostname()
      name = List.to_string(name)
      user = HexWeb.User.get(username: "user")
      key = HexWeb.API.Key.get(name, user)

      assert Hex.Mix.read_config[:username] == "user"
      assert Hex.Mix.read_config[:key] == key.secret
    end
  end

  test "list keys" do
    user = HexWeb.User.get(username: "user")
    HexWeb.API.Key.create("list_keys", user)

    Mix.Tasks.Hex.Key.run(["list", "-u", "user", "-p", "hunter42"])
    assert_received {:mix_shell, :info, ["list_keys"]}
  end

  test "remove key" do
    user = HexWeb.User.get(username: "user")
    HexWeb.API.Key.create("drop_key", user)

    Mix.Tasks.Hex.Key.run(["remove", "drop_key", "-u", "user", "-p", "hunter42"])

    assert_received {:mix_shell, :info, ["Removing key drop_key..."]}
    refute HexWeb.API.Key.get("drop_key", user)
  end
end
