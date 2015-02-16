defmodule Mix.Tasks.Hex.RootTest do
  use HexTest.Case
  @moduletag :integration

  test "general" do
    in_tmp fn ->
      Hex.Registry.start!(registry_path: tmp_path("registry.ets"))
      Hex.home(System.cwd!)
      HexWeb.RegistryBuilder.rebuild

      assert {200, data} = Hex.API.Registry.get
      File.write!(Hex.Registry.path, :zlib.gunzip(data))
      Mix.Tasks.Hex.run([])

      message = "Hex v" <> Hex.version
      assert_received {:mix_shell, :info, [^message]}
      assert_received {:mix_shell, :info, ["Registry file available (last updated: " <> _]}
      assert_received {:mix_shell, :info, ["Size: " <> _]}
      assert_received {:mix_shell, :info, ["Packages #: " <> _]}
      assert_received {:mix_shell, :info, ["Versions #: " <> _]}
    end
  end
end
