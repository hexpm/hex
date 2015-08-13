defmodule Hex.ResolverTest do
  use HexTest.Case

  defp resolve(reqs, locked \\ []) do
    case Hex.Resolver.resolve(reqs(reqs), deps(reqs), locked(locked)) do
      {:ok, dict} -> dict
      {:error, messages} -> messages
    end
  end

  defp deps(reqs) do
    Enum.map(reqs, fn {app, _req} ->
      %Mix.Dep{app: app, opts: [hex: app]}
    end)
    end

  defp reqs(reqs) do
    Enum.map(reqs, fn {app, req} ->
      name = Atom.to_string(app)
      {name, name, req, "mix.exs"}
    end)
  end

  defp locked(locked) do
    Enum.map(locked, fn {app, req} ->
      name = Atom.to_string(app)
      {name, name, req}
    end)
  end

  setup do
    Hex.Registry.open!(registry_path: tmp_path("registry.ets"))
  end

  test "simple" do
    deps = [foo: nil, bar: nil]
    assert Dict.equal? locked([foo: "0.2.1", bar: "0.2.0"]), resolve(deps)

    deps = [foo: "0.2.1", bar: "0.2.0"]
    assert Dict.equal? locked([foo: "0.2.1", bar: "0.2.0"]), resolve(deps)

    deps = [foo: "0.2.0", bar: "0.2.0"]
    assert Dict.equal? locked([foo: "0.2.0", bar: "0.2.0"]), resolve(deps)

    deps = [foo: "~> 0.3.0", bar: nil]
    assert resolve(deps) == "Looking up alternatives for conflicting requirements on foo\n  From mix.exs: ~> 0.3.0"

    deps = [foo: nil, bar: "~> 0.3.0"]
    assert resolve(deps) == "Looking up alternatives for conflicting requirements on bar\n  From mix.exs: ~> 0.3.0"
  end

  test "backtrack" do
    deps = [decimal: "0.2.0", ex_plex: "0.2.0"]
    assert Dict.equal? locked([decimal: "0.2.0", ex_plex: "0.2.0"]), resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: ">= 0.1.0"]
    assert Dict.equal? locked([decimal: "0.1.0", ex_plex: "0.1.2"]), resolve(deps)

    deps = [decimal: nil, ex_plex: "< 0.1.0"]
    assert Dict.equal? locked([decimal: "0.2.1", ex_plex: "0.0.1"]), resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: "< 0.1.0"]
    assert Dict.equal? locked([decimal: "0.1.0", ex_plex: "0.0.1"]), resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: "~> 0.0.2"]
    assert resolve(deps) == "Looking up alternatives for conflicting requirements on decimal\n  Activated version: 0.1.0\n  From ex_plex v0.0.2: 0.1.1\n  From mix.exs: 0.1.0"

    deps = [decimal: nil, ex_plex: "0.0.2"]
    assert resolve(deps) == "Looking up alternatives for conflicting requirements on decimal\n  Activated version: 0.2.1\n  From ex_plex v0.0.2: 0.1.1\n  From mix.exs: "
  end

  test "complete backtrack" do
    deps = [jose: nil, eric: nil]
    assert Dict.equal? locked([jose: "0.2.1", eric: "0.0.2"]), resolve(deps)
  end

  test "locked" do
    locked = [decimal: "0.2.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? locked([decimal: "0.2.0", ex_plex: "0.2.0"]), resolve(deps, locked)

    locked = [decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? locked([decimal: "0.1.0", ex_plex: "0.1.2"]), resolve(deps, locked)

    locked = [decimal: "0.0.1"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? locked([decimal: "0.0.1", ex_plex: "0.0.1"]), resolve(deps, locked)

    locked = [ex_plex: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert Dict.equal? locked([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert Dict.equal? locked([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? locked([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = []
    assert Dict.equal? [], resolve(deps, locked)
  end

  test "failure due to locked dep" do
    locked = [decimal: "0.0.1"]
    deps = [decimal: nil, ex_plex: "0.1.0" ]
    assert resolve(deps, locked) == "Looking up alternatives for conflicting requirements on decimal\n  Activated version: 0.0.1\n  From ex_plex v0.1.0: ~> 0.1.0\n  From mix.exs: \n  From mix.lock: 0.0.1"

    locked = [decimal: "0.0.1"]
    deps = [decimal: "~> 0.0.1", ex_plex: "0.1.0" ]
    assert resolve(deps, locked) == "Looking up alternatives for conflicting requirements on decimal\n  Activated version: 0.0.1\n  From ex_plex v0.1.0: ~> 0.1.0\n  From mix.exs: ~> 0.0.1\n  From mix.lock: 0.0.1"
  end

  test "optional" do
    deps = [ex_doc: nil, has_optional: nil]
    assert Dict.equal? locked([ex_doc: "0.0.2", has_optional: "0.1.0"]), resolve(deps)
  end
end
