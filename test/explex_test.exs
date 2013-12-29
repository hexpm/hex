defmodule ExplexTest do
  use ExUnit.Case, async: false

  @dets_table :explex_dets_registry

  defp create_registry(packages) do
    dets_opts = [
      file: Path.join(tmp_path, "explex.dets"),
      ram_file: true,
      type: :duplicate_bag ]

    { :ok, @dets_table } = :dets.open_file(@dets_table, dets_opts)
    :ok = :dets.insert(@dets_table, packages)
    :ok = :dets.close(@dets_table)
  end

  defp tmp_path do
    Path.expand("../tmp", __DIR__)
  end

  setup_all do
    File.rm_rf!(tmp_path)
    File.mkdir_p!(tmp_path)

    create_registry [
      { :foo, "0.0.1", [] },
      { :foo, "0.1.0", [] },
      { :foo, "0.2.0", [] },
      { :foo, "0.2.1", [] },
      { :bar, "0.0.1", [] },
      { :bar, "0.1.0", [foo: "~> 0.1.0"] },
      { :bar, "0.2.0", [foo: "~> 0.2.0"] },

      { :decimal, "0.0.1", [] },
      { :decimal, "0.1.0", [] },
      { :decimal, "0.2.0", [] },
      { :decimal, "0.2.1", [] },
      { :ex_plex, "0.0.1", [] },
      { :ex_plex, "0.0.2", [decimal: "0.1.1"] },
      { :ex_plex, "0.1.0", [decimal: "~> 0.1.0"] },
      { :ex_plex, "0.1.2", [decimal: "~> 0.1.0"] },
      { :ex_plex, "0.2.0", [decimal: "~> 0.2.0"] },

      { :jose, "0.2.0", [] },
      { :jose, "0.2.1", [] },
      { :eric, "0.0.1", [] },
      { :eric, "0.0.2", [] },
      { :eric, "0.1.0", [jose: "~> 0.1.0"] },
      { :eric, "0.1.2", [jose: "~> 0.1.0"] },
      { :eric, "0.2.0", [jose: "~> 0.3.0"] },

      { :ex_unit, "0.0.1", [] },
      { :ex_unit, "0.0.2", [] },
      { :ex_unit, "0.1.0", [] },
      { :postgrex, "0.2.0", [ex_unit: "0.0.1"] },
      { :postgrex, "0.2.1", [ex_unit: "~> 0.1.0"] },
      { :ecto, "0.2.0", [postgrex: "~> 0.2.0", ex_unit: "~> 0.0.1"] },
      { :ecto, "0.2.1", [postgrex: "~> 0.2.0", ex_unit: "0.0.2"] },
    ]
    :ok
  end

  setup do
    Explex.start [
      registry_path: Path.join(tmp_path, "explex.dets"),
      ram_file: true ]
    :ok
  end

  teardown do
    Explex.stop
    :ok
  end

  test "simple" do
    deps = [foo: nil, bar: nil]
    assert Dict.equal? [foo: "0.2.1", bar: "0.2.0"], Explex.resolve(deps)

    deps = [foo: "0.2.1", bar: "0.2.0"]
    assert Dict.equal? [foo: "0.2.1", bar: "0.2.0"], Explex.resolve(deps)

    deps = [foo: "0.2.0", bar: "0.2.0"]
    assert Dict.equal? [foo: "0.2.0", bar: "0.2.0"], Explex.resolve(deps)

    deps = [foo: "~> 0.3.0", bar: nil]
    assert nil = Explex.resolve(deps)

    deps = [foo: nil, bar: "~> 0.3.0"]
    assert nil = Explex.resolve(deps)
  end

  test "backtrack" do
    deps = [decimal: "0.2.0", ex_plex: "0.2.0"]
    assert Dict.equal? [decimal: "0.2.0", ex_plex: "0.2.0"], Explex.resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: ">= 0.1.0"]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.2"], Explex.resolve(deps)

    deps = [decimal: nil, ex_plex: "< 0.1.0"]
    assert Dict.equal? [decimal: "0.2.1", ex_plex: "0.0.1"], Explex.resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: "< 0.1.0"]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.0.1"], Explex.resolve(deps)

    deps = [decimal: "0.1.0", ex_plex: "~> 0.0.2"]
    assert nil = Explex.resolve(deps)

    deps = [decimal: nil, ex_plex: "0.0.2"]
    assert nil = Explex.resolve(deps)
  end

  test "complete backtrack" do
    deps = [jose: nil, eric: nil]
    assert Dict.equal? [jose: "0.2.1", eric: "0.0.2"], Explex.resolve(deps)
  end

  test "more backtrack" do
    deps = [ecto: nil]
    assert Dict.equal? [ecto: "0.2.0", postgrex: "0.2.0", ex_unit: "0.0.1"], Explex.resolve(deps)
  end

  test "locked" do
    locked = [decimal: "0.2.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.2.0", ex_plex: "0.2.0"], Explex.resolve(deps, locked)

    locked = [decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.2"], Explex.resolve(deps, locked)

    locked = [decimal: "0.0.1"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.0.1", ex_plex: "0.0.1"], Explex.resolve(deps, locked)

    locked = [ex_plex: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], Explex.resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: "0.1.0", ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], Explex.resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = [decimal: nil, ex_plex: nil]
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], Explex.resolve(deps, locked)

    locked = [ex_plex: "0.1.0", decimal: "0.1.0"]
    deps = []
    assert Dict.equal? [decimal: "0.1.0", ex_plex: "0.1.0"], Explex.resolve(deps, locked)

    locked = [ex_plex: "0.1.0"]
    deps = [decimal: "~> 0.2.0", ex_plex: nil]
    assert nil = Explex.resolve(deps, locked)
  end
end
