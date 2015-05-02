defmodule Mix.Tasks.Hex.KeyTest do
  use HexTest.Case
  @moduletag :integration

  test "list keys" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      user = HexWeb.User.get(username: "user")
      {:ok, key} = HexWeb.API.Key.create(user, %{"name" => "list_keys"})
      Hex.Config.update([key: key.user_secret])

      Mix.Tasks.Hex.Key.run(["list"])
      assert_received {:mix_shell, :info, ["list_keys"]}
    end
  end

  test "remove key" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      user = HexWeb.User.get(username: "user")
      {:ok, key} = HexWeb.API.Key.create(user, %{"name" => "drop_key"})
      Hex.Config.update([key: key.user_secret])

      Mix.Tasks.Hex.Key.run(["remove", "drop_key"])

      assert_received {:mix_shell, :info, ["Removing key drop_key..."]}
      refute HexWeb.API.Key.get("drop_key", user)
    end
  end
end
