defmodule Mix.Tasks.Hex.KeyTest do
  use HexTest.Case
  @moduletag :integration

  test "list keys" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth = HexTest.HexWeb.new_user("list_keys", "list_keys@mail.com", "password", "list_keys")
      Hex.Config.update(auth)

      assert {200, [%{"name" => "list_keys"}], _} = Hex.API.Key.get(auth)

      Mix.Tasks.Hex.Key.run(["list"])
      assert_received {:mix_shell, :info, ["list_keys"]}
    end
  end

  test "remove key" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      auth = HexTest.HexWeb.new_user("remove_key", "remove_key@mail.com", "password", "remove_key")
      Hex.Config.update(auth)

      assert {200, _, _} = Hex.API.Key.get(auth)

      Mix.Tasks.Hex.Key.run(["remove", "remove_key"])
      assert_received {:mix_shell, :info, ["Removing key remove_key..."]}

      assert {401, _, _} = Hex.API.Key.get(auth)
    end
  end
end
