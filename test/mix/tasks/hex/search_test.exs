defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Mix.Tasks.Hex.Search.run(["doc"])
    assert_received {:mix_shell, :info, ["ex_doc\e[0m    http://localhost:4043/packages/ex_doc" <> _]}
    assert_received {:mix_shell, :info, ["only_doc\e[0m  http://localhost:4043/packages/only_doc" <> _]}
  end

  test "empty search" do
    Mix.Tasks.Hex.Search.run(["bloopdoopbloop"])
    assert_received {:mix_shell, :info, ["No packages found"]}
  end
end
