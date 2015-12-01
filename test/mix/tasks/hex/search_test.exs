defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Hex.Registry.open!(registry_path: tmp_path("registry.ets"))
    Hex.State.put(:home, tmp_path())

    Mix.Tasks.Hex.Search.run(["ex"])
    assert_received {:mix_shell, :info, ["ex_doc   v0.1.0"]}
    assert_received {:mix_shell, :info, ["ex_plex  v0.2.0"]}
    assert_received {:mix_shell, :info, ["postgrex v0.2.1"]}
  end
end
