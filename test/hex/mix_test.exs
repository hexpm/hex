defmodule Hex.MixTest do
  use HexTest.Case, async: true

  test "from mixlock" do
    lock = [ ex_doc: {:hex, :ex_doc, "0.1.0"},
             postgrex: {:hex, :fork, "0.2.1"} ]
    assert Hex.Mix.from_lock(lock) ==
           [{"ex_doc", "ex_doc", "0.1.0"}, {"fork", "postgrex", "0.2.1"}]
  end

  test "flatten_deps with only dependencies" do
    child_dep_1 = %Mix.Dep{app: :child_dep_1, deps: [], top_level: false}
    child_dep_2 = %Mix.Dep{app: :child_dep_2, deps: [], top_level: false}
    dev_child_dep = %Mix.Dep{app: :dev_child_dep, deps: [], top_level: false, opts: [only: :dev]}
    dep = %Mix.Dep{app: :dep, deps: [child_dep_1, child_dep_2, dev_child_dep], top_level: true}

    deps = [dep, child_dep_1, child_dep_2]

    flattened_deps = Hex.Mix.flatten_deps(deps, [:dep])
    assert dep in flattened_deps
    assert child_dep_1 in flattened_deps
    assert child_dep_2 in flattened_deps
    refute dev_child_dep in flattened_deps
  end
end
