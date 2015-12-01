defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  setup do
    Hex.State.put(:registry_updated, true)
    Hex.Registry.open!(registry_path: tmp_path("registry.ets"))
  end

  test "search" do
    Mix.Tasks.Hex.Search.run(["ex"])
    assert_received {:mix_shell, :info, ["ex_doc"]}
    assert_received {:mix_shell, :info, ["ex_plex"]}
    assert_received {:mix_shell, :info, ["postgrex"]}
  end
end
