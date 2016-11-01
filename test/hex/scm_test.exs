defmodule Hex.SCMTest do
  use ExUnit.Case, async: true

  test "guess build_tools" do
    empty_meta    = %{}
    guessed_meta  = %{"build_tools" => ["mix"]}
    no_tools_meta = %{"files" => ["README.md"]}
    tools_meta    = %{"files" => ["README.md", "mix.exs", "lib"]}

    assert [] = Hex.SCM.guess_build_tools(empty_meta)
    assert ["mix"] = Hex.SCM.guess_build_tools(guessed_meta)
    assert [] = Hex.SCM.guess_build_tools(no_tools_meta)
    assert ["mix"] = Hex.SCM.guess_build_tools(tools_meta)
  end
end
