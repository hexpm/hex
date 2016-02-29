defmodule Hex.Resolver.BacktracksTest do
  use HexTest.Case
  import Hex.Resolver.Backtracks, only: [message: 1]

  setup do
    Hex.Registry.open!(Hex.Registry.ETS, registry_path: tmp_path("registry.ets"))
  end

  test "merge versions" do
    assert message({"foo", ["0.0.1", "0.1.0", "0.2.0"], []}) ==
           "Conflict on foo from 0.0.1 to 0.2.0\n  "

    assert message({"foo", ["0.0.1", "0.1.0", "0.2.1"], []}) ==
           "Conflict on foo 0.0.1, 0.1.0, 0.2.1\n  "

    assert message({"foo", ["0.1.0", "0.2.1"], []}) ==
           "Conflict on foo 0.1.0, 0.2.1\n  "
  end
end
