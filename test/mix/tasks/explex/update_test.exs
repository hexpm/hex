defmodule Mix.Tasks.Explex.UpdateTest do
  use ExplexTest.Case
  @moduletag :integration

  test "fetch registry" do
    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)
      refute File.exists?("registry.dets")

      Mix.Tasks.Explex.Update.run([])
      assert_received { :mix_shell, :info, ["Downloading registry..."] }
      assert_received { :mix_shell, :info, ["Updating registry was successful!"] }
      assert File.exists?("registry.dets")

      Mix.Tasks.Explex.Update.run([])
      assert_received { :mix_shell, :info, ["Downloading registry..."] }
      assert_received { :mix_shell, :info, ["Updating registry was successful!"] }
      assert File.exists?("registry.dets")
    end
  after
    System.delete_env("MIX_HOME")
  end
end
