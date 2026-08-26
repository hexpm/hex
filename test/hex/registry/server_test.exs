defmodule Hex.Registry.ServerTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  setup do
    Hex.State.put(:offline, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    :ok
  end

  test "dependencies/3 turns an unsatisfiable requirement into an empty constraint" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(
        tid,
        {{:deps, "hexpm", "parent", "1.0.0"},
         [{"hexpm", "child", "child", "~> 1.0 and >= 2.0.0", false}]}
      )

      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)

    assert {:ok, [dependency]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    assert dependency.constraint == %Hex.Solver.Constraints.Empty{}

    assert_received {:mix_shell, :error,
                     [
                       "\e[33mPackage parent 1.0.0 can't be used because its requirement " <>
                         "\"~> 1.0 and >= 2.0.0\" for child can never be satisfied: " <>
                         "\"~> 1.0\" and \">= 2.0.0\" are disjoint" <> _
                     ]}

    assert {:ok, [_dependency]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    refute_received {:mix_shell, :error, _}
  end

  test "prefetch raises a helpful error in offline mode when a package is not cached" do
    assert_raise Mix.Error,
                 ~r"Hex is running in offline mode and the registry entry for package missing_package is not cached locally",
                 fn ->
                   Registry.prefetch([{"hexpm", "missing_package"}])
                 end
  end
end
