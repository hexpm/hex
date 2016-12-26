defmodule Mix.Tasks.Hex.ConfigTest do
  use HexTest.Case

  test "config" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      assert_raise Mix.Error, "Config does not contain key foo", fn ->
        Mix.Tasks.Hex.Config.run(["foo"])
      end

      Mix.Tasks.Hex.Config.run(["foo", "bar"])
      Mix.Tasks.Hex.Config.run(["foo"])
      assert_received {:mix_shell, :info, ["\"bar\""]}

      Mix.Tasks.Hex.Config.run(["foo", "--delete"])
      assert_raise Mix.Error, "Config does not contain key foo", fn ->
        Mix.Tasks.Hex.Config.run(["foo"])
      end
    end
  end

  test "direct api" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      assert Hex.Config.read == []

      Hex.Config.update([key: "value"])
      assert Hex.Config.read == [key: "value"]

      Hex.Config.update([key: "other", foo: :bar])
      assert Hex.Config.read == [key: "other", foo: :bar]
    end
  end
end
