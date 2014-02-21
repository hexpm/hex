defmodule Mix.Tasks.Explex.SearchTest do
  use ExplexTest.Case
  @moduletag :integration

  test "search" do
    Mix.Tasks.Explex.Search.run(["e"])
    assert_received { :mix_shell, :info, ["ex_doc"] }
  end
end
