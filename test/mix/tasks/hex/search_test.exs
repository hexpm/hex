defmodule Mix.Tasks.Hex.SearchTest do
  use HexTest.Case
  @moduletag :integration

  test "search" do
    Mix.Tasks.Hex.Search.run(["doc"])
    assert_received {:mix_shell, :info, ["ex_doc\e[0m    https://hex.pm/packages/ex_doc" <> _]}
    assert_received {:mix_shell, :info, ["only_doc\e[0m  https://hex.pm/packages/only_doc" <> _]}
  end

  test "empty search" do
    Mix.Tasks.Hex.Search.run(["bloopdoopbloop"])
    assert_received {:mix_shell, :info, ["No packages found"]}
  end
end
