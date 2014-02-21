defmodule Hex.Test do
  use HexTest.Case

  import Hex.Resolver

  setup do
    Hex.Registry.start [
      registry_path: tmp_path("hex.dets"),
      ram_file: true ]
  end

  test "from mixlock" do
    lock = [ ex_doc: { :git, fixture_path("ex_doc-0.1.0"), "HEAD", [] },
             postgrex: { :git, fixture_path("postgrex-0.2.1"), "HEAD", [] } ]
    assert [ex_doc: "0.1.0", postgrex: "0.2.1"] = Hex.Mix.from_lock(lock)
  end

  test "simple" do
    deps = [foo: nil, bar: nil]
    assert Dict.equal? [foo: "0.2.1", bar: "0.2.0"], resolve(deps)

    deps = [foo: "0.2.1", bar: "0.2.0"]
    assert Dict.equal? [foo: "0.2.1", bar: "0.2.0"], resolve(deps)

    deps = [foo: "0.2.0", bar: "0.2.0"]
    assert Dict.equal? [foo: "0.2.0", bar: "0.2.0"], resolve(deps)

    deps = [foo: "~> 0.3.0", bar: nil]
    assert nil = resolve(deps)

    deps = [foo: nil, bar: "~> 0.3.0"]
    assert nil = resolve(deps)
  end

  test "backtrack" do
    deps = [decimal: "0.2.0", ex_plex: "0.2.0"]
    assert Dict.equal? [decimal: "0.2.0", ex_plex: "0.2.0"], resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: ">= 0.1.0"]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.2"], resolve(deps)

    deps = [decimal: nil, ex_plex: "< 0.1.0"]
    assert Dict.equal? [decimal: "0.2.1", ex_plex: "0.0.1"], resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: "< 0.1.0"]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.0.1"], resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: "~> 0.0.2"]
    assert nil = resolve(deps)

    deps = [decimal: nil, ex_plex: "0.0.2"]
    assert nil = resolve(deps)
  end

  test "complete backtrack" do
    deps = [jose: nil, eric: nil]
    assert Dict.equal? [jose: "0.2.1", eric: "0.0.2"], resolve(deps)
  end

  test "more backtrack" do
    deps = [ecto: nil]
    assert Dict.equal? [ecto: "0.2.0", postgrex: "0.2.0", ex_doc: "0.0.1"], resolve(deps)
  end

  test "locked" do
    locked = [decimal: "0.2.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.2.0", ex_plex: "0.2.0"], resolve(deps, locked)

    locked = [decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.2"], resolve(deps, locked)

    locked = [decimal: "0.0.1"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.0.1", ex_plex: "0.0.1"], resolve(deps, locked)

    locked = [ex_plex: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = []
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], resolve(deps, locked)

    locked = [ex_plex: "0.1.0"]
    deps = [decimal: "~> 0.2.0", ex_plex: nil]
    assert nil = resolve(deps, locked)
  end
end
