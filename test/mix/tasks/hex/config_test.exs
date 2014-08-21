defmodule Mix.Tasks.Hex.ConfigTest do
  use HexTest.Case

  test "config" do
    in_tmp fn ->
      Hex.home(System.cwd!)

      assert_raise Mix.Error, "Config does not contain a key foo", fn ->
        Mix.Tasks.Hex.Config.run(["foo"])
      end

      Mix.Tasks.Hex.Config.run(["foo", "bar"])
      Mix.Tasks.Hex.Config.run(["foo"])
      assert_received {:mix_shell, :info, ["\"bar\""]}
    end
  end

  test "direct api" do
    in_tmp fn ->
      Hex.home(System.cwd!)
      assert Hex.Util.read_config == []

      Hex.Util.update_config([key: "value"])
      assert Hex.Util.read_config == [key: "value"]

      Hex.Util.update_config([key: "other", foo: :bar])
      assert Hex.Util.read_config == [key: "other", foo: :bar]
    end
  end

  test "nuke config" do
    in_tmp fn ->
      Hex.home(System.cwd!)
      assert Hex.Util.read_config == []

      Hex.Util.update_config([key: "value"])
      assert Hex.Util.read_config == [key: "value"]

      Hex.Util.update_config([key: "other", foo: :bar])
      assert Hex.Util.read_config == [key: "other", foo: :bar]

      Hex.Util.nuke_config()

      assert Hex.Util.read_config == []
    end
  end
end
