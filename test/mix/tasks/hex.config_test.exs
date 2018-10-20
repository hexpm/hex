defmodule Mix.Tasks.Hex.ConfigTest do
  use HexTest.Case

  test "config" do
    in_tmp(fn ->
      Hex.State.put(:home, System.cwd!())

      assert_raise Mix.Error, "Config does not contain key offline", fn ->
        Mix.Tasks.Hex.Config.run(["offline"])
      end

      Mix.Tasks.Hex.Config.run(["offline", "true"])
      Mix.Tasks.Hex.Config.run(["offline"])
      assert_received {:mix_shell, :info, ["\"true\""]}

      Mix.Tasks.Hex.Config.run(["offline", "--delete"])

      assert_raise Mix.Error, "Config does not contain key offline", fn ->
        Mix.Tasks.Hex.Config.run(["offline"])
      end

      assert_raise Mix.Error, "Invalid key foo", fn ->
        Mix.Tasks.Hex.Config.run(["foo", "bar"])
      end
    end)
  end

  test "direct api" do
    in_tmp(fn ->
      Hex.State.put(:home, System.cwd!())
      assert Hex.Config.read() == []

      Hex.Config.update(key: "value")
      assert Hex.Config.read() == [key: "value"]

      Hex.Config.update(key: "other", foo: :bar)
      assert Hex.Config.read() == [key: "other", foo: :bar]
    end)
  end
end
