defmodule Mix.Tasks.Hex.InfoTest do
  use HexTest.Case
  @moduletag :integration

  test "package" do
    Mix.Tasks.Hex.Info.run(["ex_doc"])
    assert_received {:mix_shell, :info, ["Builds docs\n"]}
    assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.1.0\"}"]}
    assert_received {:mix_shell, :info, ["Maintainers: John Doe, Jane Doe"]}

    Mix.Tasks.Hex.Info.run(["no_package"])
    assert_received {:mix_shell, :error, ["No package with name no_package"]}
  end

  test "release" do
    Mix.Tasks.Hex.Info.run(["ex_doc", "0.0.1"])
    assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.0.1\"}"]}

    Mix.Tasks.Hex.Info.run(["ex_doc", "0.1.0-rc1"])
    assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.1.0-rc1\"}"]}

    Mix.Tasks.Hex.Info.run(["ex_doc", "1.2.3"])
    assert_received {:mix_shell, :error, ["No release with name ex_doc 1.2.3"]}
  end

  test "general" do
    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      assert {200, data, _} = Hex.API.Registry.get
      File.write!(Hex.Registry.ETS.path <> ".gz", data)
      File.write!(Hex.Registry.ETS.path, :zlib.gunzip(data))
      Mix.Tasks.Hex.Info.run([])

      message = "Hex:    " <> Hex.version
      assert_received {:mix_shell, :info, [^message]}
      assert_received {:mix_shell, :info, ["Registry file available (last updated: " <> _]}
      assert_received {:mix_shell, :info, ["File size:   " <> _]}
      assert_received {:mix_shell, :info, ["Packages #:  " <> _]}
      assert_received {:mix_shell, :info, ["Versions #:  " <> _]}
    end
  end
end
