defmodule Hex.ResolverTest do
  use HexTest.Case

  defp resolve(reqs, locked \\ []) do
    Hex.Resolver.resolve(reqs(reqs), [], reqs(locked))
  end

  def reqs(reqs) do
    Enum.map(reqs, fn
      {app, req} ->
        name = Atom.to_string(app)
        {name, name, req}
      {name, name, req} ->
        {name, name, req}
    end)
  end

  setup do
    Hex.Registry.start!(registry_path: tmp_path("registry.ets"))
    Application.put_env(:hex, :registry_updated, true)
  end

  test "simple" do
    deps = reqs([foo: nil, bar: nil])
    assert Dict.equal? reqs([foo: "0.2.1", bar: "0.2.0"]), resolve(deps)

    deps = reqs([foo: "0.2.1", bar: "0.2.0"])
    assert Dict.equal? reqs([foo: "0.2.1", bar: "0.2.0"]), resolve(deps)

    deps = reqs([foo: "0.2.0", bar: "0.2.0"])
    assert Dict.equal? reqs([foo: "0.2.0", bar: "0.2.0"]), resolve(deps)

    deps = reqs([foo: "~> 0.3.0", bar: nil])
    assert nil = resolve(deps)

    deps = reqs([foo: nil, bar: "~> 0.3.0"])
    assert nil = resolve(deps)
  end

  test "backtrack" do
    deps = reqs([decimal: "0.2.0", ex_plex: "0.2.0"])
    assert Dict.equal? reqs([decimal: "0.2.0", ex_plex: "0.2.0"]), resolve(deps)

    deps = reqs([decimal: "0.1.0", ex_plex: ">= 0.1.0"])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.1.2"]), resolve(deps)

    deps = reqs([decimal: nil, ex_plex: "< 0.1.0"])
    assert Dict.equal? reqs([decimal: "0.2.1", ex_plex: "0.0.1"]), resolve(deps)

    deps = reqs([decimal: "0.1.0", ex_plex: "< 0.1.0"])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.0.1"]), resolve(deps)

    deps = reqs([decimal: "0.1.0", ex_plex: "~> 0.0.2"])
    assert nil = resolve(deps)

    deps = reqs([decimal: nil, ex_plex: "0.0.2"])
    assert nil = resolve(deps)
  end

  test "complete backtrack" do
    deps = reqs([jose: nil, eric: nil])
    assert Dict.equal? reqs([jose: "0.2.1", eric: "0.0.2"]), resolve(deps)
  end

  test "locked" do
    locked = reqs([decimal: "0.2.0"])
    deps = reqs([decimal: nil, ex_plex: nil])
    assert Dict.equal? reqs([decimal: "0.2.0", ex_plex: "0.2.0"]), resolve(deps, locked)

    locked = reqs([decimal: "0.1.0"])
    deps = reqs([decimal: nil, ex_plex: nil])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.1.2"]), resolve(deps, locked)

    locked = reqs([decimal: "0.0.1"])
    deps = reqs([decimal: nil, ex_plex: nil])
    assert Dict.equal? reqs([decimal: "0.0.1", ex_plex: "0.0.1"]), resolve(deps, locked)

    locked = reqs([ex_plex: "0.1.0"])
    deps = reqs([decimal: "0.1.0", ex_plex: nil])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)

    locked = reqs([ex_plex: "0.1.0", decimal: "0.1.0"])
    deps = reqs([decimal: "0.1.0", ex_plex: nil])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)

    locked = reqs([ex_plex: "0.1.0", decimal: "0.1.0"])
    deps = reqs([decimal: nil, ex_plex: nil])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)

    locked = reqs([ex_plex: "0.1.0", decimal: "0.1.0"])
    deps = reqs([])
    assert Dict.equal? reqs([decimal: "0.1.0", ex_plex: "0.1.0"]), resolve(deps, locked)
  end
end
