defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Hex.Registry.start!(registry_path: tmp_path("registry.ets"))
    Hex.home(tmp_path())

    Mix.Tasks.Hex.Search.run(["ex"])
    assert_received {:mix_shell, :info, ["ex_doc"]}
    assert_received {:mix_shell, :info, ["ex_plex"]}
    assert_received {:mix_shell, :info, ["postgrex"]}
  end
end
