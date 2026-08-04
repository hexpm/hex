defmodule Hex.MixTest do
  use HexTest.Case

  test "to_lock/1 persists unconstrained dependencies as valid requirements" do
    Hex.Registry.Server.open()
    put_dependency("unconstrained_parent", "1.0.0", ">= 0.5.50 or < 0.9.0", false)

    lock =
      Hex.Mix.to_lock([
        {"hexpm", "unconstrained_parent", "unconstrained_parent", "1.0.0"}
      ])

    {:hex, :unconstrained_parent, "1.0.0", _inner_checksum, _managers, deps, "hexpm",
     _outer_checksum} = lock[:unconstrained_parent]

    assert [{:foo, requirement, hex: :foo, repo: "hexpm", optional: false}] = deps
    assert requirement == ">= 0.0.0-0"
    assert {:ok, _requirement} = Version.parse_requirement(requirement)
    assert Hex.Solver.parse_constraint!(requirement) == %Hex.Solver.Constraints.Range{}
  end

  test "to_lock/1 persists empty optional dependencies as valid requirements" do
    Hex.Registry.Server.open()
    put_dependency("empty_parent", "1.0.0", "< 0.0.0-0", true)

    lock = Hex.Mix.to_lock([{"hexpm", "empty_parent", "empty_parent", "1.0.0"}])

    {:hex, :empty_parent, "1.0.0", _inner_checksum, _managers, deps, "hexpm", _outer_checksum} =
      lock[:empty_parent]

    assert [{:foo, requirement, hex: :foo, repo: "hexpm", optional: true}] = deps
    assert requirement == "< 0.0.0-0"
    assert {:ok, _requirement} = Version.parse_requirement(requirement)
    assert Hex.Solver.parse_constraint!(requirement) == %Hex.Solver.Constraints.Empty{}
  end

  test "from_lock/1" do
    lock = [ex_doc: {:hex, :ex_doc, "0.1.0"}, postgrex: {:hex, :fork, "0.2.1"}]

    assert Hex.Mix.from_lock(lock) ==
             [
               %{repo: "hexpm", name: "ex_doc", app: "ex_doc", version: "0.1.0"},
               %{repo: "hexpm", name: "fork", app: "postgrex", version: "0.2.1"}
             ]
  end

  test "from_lock/1 warns on newer lock versions" do
    message =
      {:mix_shell, :error,
       [
         "\e[33mThe mix.lock file was generated with a newer version of Hex. " <>
           "Update your client by running `mix local.hex` to avoid losing data.\e[0m"
       ]}

    lock = [
      ex_doc:
        {:hex, :ex_doc, "0.1.0", "checksum", [:mix], [{:dep, ">= 0.0.0", [hex: :dep]}], "hexpm",
         "checksum"}
    ]

    Hex.Server.reset()
    Hex.Mix.from_lock(lock)
    refute_received ^message

    lock = [
      ex_doc:
        {:hex, :ex_doc, "0.1.0", "checksum", [:mix], [{:dep, ">= 0.0.0", [hex: :dep]}], "hexpm",
         "checksum", "entry from newer version"}
    ]

    Hex.Server.reset()
    Hex.Mix.from_lock(lock)
    assert_received ^message

    Hex.Mix.from_lock(lock)
    refute_received ^message
  end

  defp put_dependency(package, version, requirement, optional) do
    :sys.replace_state(Hex.Registry.Server, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(tid, [
        {{:inner_checksum, "hexpm", package, version}, <<0::256>>},
        {{:outer_checksum, "hexpm", package, version}, <<1::256>>},
        {{:deps, "hexpm", package, version}, [{"hexpm", "foo", "foo", requirement, optional}]}
      ])

      %{state | fetched: MapSet.put(fetched, {"hexpm", package})}
    end)
  end
end
