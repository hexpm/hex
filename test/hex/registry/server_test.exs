defmodule Hex.Registry.ServerTest do
  use HexTest.Case
  alias Hex.Registry.Server, as: Registry

  setup do
    Hex.State.put(:offline, true)
    Registry.open(registry_path: tmp_path("cache.ets"))
    :ok
  end

  test "app_dependencies/3 accepts application names" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(
        tid,
        {{:deps, "hexpm", "parent", "1.0.0"},
         [
           {"hexpm", "child", "child_2", "~> 1.0", false},
           {"hexpm", "b58", "B58", "~> 1.0", false},
           {"hexpm", "lfe_dep", "lfe-Dep", "~> 1.0", false}
         ]}
      )

      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)

    assert {:ok, [%{label: "child_2"}, %{label: "B58"}, %{label: "lfe-Dep"}]} =
             Registry.app_dependencies("hexpm", "parent", "1.0.0")
  end

  test "app_dependencies/3 rejects application names that aren't directory names" do
    for app <- [
          "../child",
          "child/..",
          "child\\..",
          "..",
          ".",
          ".child",
          "",
          "_child",
          "1child",
          "-child"
        ] do
      :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
        :ets.insert(
          tid,
          {{:deps, "hexpm", "parent", "1.0.0"}, [{"hexpm", "child", app, "~> 1.0", false}]}
        )

        %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
      end)

      message =
        "Package parent 1.0.0 can't be used because its dependency child " <>
          "has the invalid application name #{inspect(app)}"

      assert_raise Mix.Error, message, fn ->
        Registry.app_dependencies("hexpm", "parent", "1.0.0")
      end
    end
  end

  test "dependencies/3 makes a dependency with an invalid application name required and unsatisfiable" do
    :sys.replace_state(Registry, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(tid, [
        {{:deps, "hexpm", "parent", "1.0.0"},
         [
           {"hexpm", "child", "../child", "~> 1.0", true},
           {"hexpm", "other", "other", "~> 1.0", false}
         ]},
        {{:deps, "hexpm", "parent", "1.1.0"}, [{"hexpm", "child", "../child", "~> 1.0", false}]}
      ])

      %{state | fetched: MapSet.put(fetched, {"hexpm", "parent"})}
    end)

    assert {:ok, [child, other]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    assert child.constraint == Hex.Solver.parse_constraint!("< 0.0.0-0")
    refute child.optional
    assert other.constraint == Hex.Solver.parse_constraint!("~> 1.0")

    assert_received {:mix_shell, :info,
                     [
                       "\e[33mPackage parent 1.0.0 can't be used because its dependency child " <>
                         "has the invalid application name \"../child\"" <> _
                     ]}

    assert {:ok, [_child, _other]} = Registry.dependencies("hexpm", "parent", "1.0.0")
    refute_received {:mix_shell, :info, _}

    assert {:ok, [_child]} = Registry.dependencies("hexpm", "parent", "1.1.0")

    assert_received {:mix_shell, :info,
                     [
                       "\e[33mPackage parent 1.1.0 can't be used because its dependency child " <>
                         "has the invalid application name \"../child\"" <> _
                     ]}
  end

  test "prefetch raises a helpful error in offline mode when a package is not cached" do
    assert_raise Mix.Error,
                 ~r"Hex is running in offline mode and the registry entry for package missing_package is not cached locally",
                 fn ->
                   Registry.prefetch([{"hexpm", "missing_package"}])
                 end
  end
end
