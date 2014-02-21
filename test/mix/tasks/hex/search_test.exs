defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Mix.Tasks.Hex.Search.run(["e"])
    assert_received { :mix_shell, :info, ["ex_doc"] }
  end
end
