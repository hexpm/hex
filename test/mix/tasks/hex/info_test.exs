defmodule Mix.Tasks.Hex.InfoTest do
  use HexTest.Case
  @moduletag :integration

  test "package" do
    Mix.Tasks.Hex.Info.run(["ex_doc"])

    assert_received { :mix_shell, :info, ["ex_doc"] }
    assert_received { :mix_shell, :info, ["  Contributors: John Doe, Jane Doe"] }
    assert_received { :mix_shell, :info, ["builds docs"] }

    Mix.Tasks.Hex.Info.run(["no_package"])
    assert_received { :mix_shell, :error, ["No package with name no_package"] }
  end

  test "release" do
    Mix.Tasks.Hex.Info.run(["ex_doc", "0.0.1"])
    assert_received { :mix_shell, :info, ["ex_doc v0.0.1"] }

    Mix.Tasks.Hex.Info.run(["ex_doc", "1.2.3"])
    assert_received { :mix_shell, :error, ["No release with name ex_doc v1.2.3"] }
  end

  test "general" do
    in_tmp fn ->
      System.put_env("MIX_HOME", System.cwd!)

      File.mkdir_p!("tmp")
      HexWeb.RegistryBuilder.sync_rebuild

      assert { 200, data } = Hex.API.get_registry
      File.write!(Hex.Registry.path, :zlib.gunzip(data))
      Mix.Tasks.Hex.Info.run([])

      message = "Hex v" <> Hex.version
      assert_received { :mix_shell, :info, [^message] }
      assert_received { :mix_shell, :info, ["Registry file available (last updated: " <> _] }
      assert_received { :mix_shell, :info, ["Size: " <> _] }
      assert_received { :mix_shell, :info, ["Packages #: " <> _] }
      assert_received { :mix_shell, :info, ["Versions #: " <> _] }
    end
  end
end
