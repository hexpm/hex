defmodule Mix.Tasks.Hex.ConfigTest do
  use HexTest.Case

  test "config" do
    in_tmp fn ->
      System.put_env("MIX_HOME", System.cwd!)

      assert_raise Mix.Error, "Config does not contain a key foo", fn ->
        Mix.Tasks.Hex.Config.run(["foo"])
      end

      Mix.Tasks.Hex.Config.run(["foo", "bar"])
      Mix.Tasks.Hex.Config.run(["foo"])
      assert_received { :mix_shell, :info, ["\"bar\""] }
    end
  after
    System.delete_env("MIX_HOME")
  end
end
