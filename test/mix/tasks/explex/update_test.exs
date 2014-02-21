defmodule Mix.Tasks.Explex.UpdateTest do
  use ExplexTest.Case
  @moduletag :integration

  test "fetch registry" do
    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)
      refute File.exists?(Explex.Registry.path)

      Mix.Tasks.Explex.Update.run([])
      assert_received { :mix_shell, :info, ["Downloading registry..."] }
      assert_received { :mix_shell, :info, ["Updating registry was successful!"] }
      assert File.exists?(Explex.Registry.path)

      Mix.Tasks.Explex.Update.run([])
      assert_received { :mix_shell, :info, ["Downloading registry..."] }
      assert_received { :mix_shell, :info, ["Updating registry was successful!"] }
      assert File.exists?(Explex.Registry.path)
    end
  after
    System.delete_env("MIX_HOME")
  end
end
