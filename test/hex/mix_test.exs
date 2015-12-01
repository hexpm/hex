defmodule Hex.MixTest do
  use HexTest.Case, async: true

  test "from mixlock" do
    lock = [ ex_doc: {:hex, :ex_doc, "0.1.0"},
             postgrex: {:hex, :fork, "0.2.1"} ]
    assert Hex.Mix.from_lock(lock) ==
           [{"ex_doc", "ex_doc", "0.1.0"}, {"postgrex", "fork", "0.2.1"}]
  end
end
