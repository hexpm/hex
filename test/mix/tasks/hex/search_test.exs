defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  setup do
    Hex.State.put(:registry_updated, true)
    Hex.Registry.open!(Hex.Registry.ETS, registry_path: tmp_path("registry.ets"))
  end

  test "search" do
    Mix.Tasks.Hex.Search.run(["ex"])
    assert_received {:mix_shell, :info, ["ex_doc   0.1.0"]}
    assert_received {:mix_shell, :info, ["ex_plex  0.2.0"]}
    assert_received {:mix_shell, :info, ["postgrex 0.2.1"]}
  end

  test "empty search" do
    Mix.Tasks.Hex.Search.run(["bloopdoopbloop"])
    assert_received {:mix_shell, :info, ["No packages found"]}
  end
end
