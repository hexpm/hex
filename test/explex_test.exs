defmodule ExplexTest do
  use ExUnit.Case, async: false
  import TestHelper

  @dets_table :explex_dets_registry

  defp create_registry(packages) do
    dets_opts = [
      file: Path.join(tmp_path, "explex.dets"),
      ram_file: true,
      type: :duplicate_bag ]

    packages =
      Enum.map(packages, fn { name, version, deps } ->
        url = "#{name}-#{version}"
        ref = "ref-#{version}"
        { name, version, deps, url, ref }
      end)

    { :ok, @dets_table } = :dets.open_file(@dets_table, dets_opts)
    :ok = :dets.insert(@dets_table, packages)
    :ok = :dets.close(@dets_table)
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

      { :ex_doc, "0.0.1", [] },
      { :ex_doc, "0.0.2", [] },
      { :ex_doc, "0.1.0", [] },
      { :postgrex, "0.2.0", [ex_doc: "0.0.1"] },
      { :postgrex, "0.2.1", [ex_doc: "~> 0.1.0"] },
      { :ecto, "0.2.0", [postgrex: "~> 0.2.0", ex_doc: "~> 0.0.1"] },
      { :ecto, "0.2.1", [postgrex: "~> 0.2.0", ex_doc: "0.0.2"] },
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

  test "package ref" do
    assert { "postgrex-0.2.1", "ref-0.2.1" } = Explex.package_ref(:postgrex, "0.2.1")
  end

  test "from mixlock" do
    lock = [ ex_doc: { :git, "ex_doc-0.1.0", "ref-0.1.0", [] },
             postgrex: { :git, "postgrex-0.2.1", "ref-0.2.1", [] } ]
    assert [ex_doc: "0.1.0", postgrex: "0.2.1"] = Explex.from_mixlock(lock)
  end

  test "to mixlock" do
    lock = [ ex_doc: { :git, "ex_doc-0.1.0", "ref-0.1.0", [] },
             postgrex: { :git, "postgrex-0.2.1", "ref-0.2.1", [] } ]
    assert ^lock = Explex.to_mixlock([ex_doc: "0.1.0", postgrex: "0.2.1"])

    lock = [ ex_doc: { :git, "ex_doc-0.1.0", "ref-0.1.0", [] },
             postgrex: { :git, "postgrex-0.2.1", "ref-0.2.1", [] } ]
    new_lock = [ ecto: { :git, "ecto-0.2.1", "ref-0.2.1", [] } ] ++ lock
    assert ^new_lock = Explex.to_mixlock([ecto: "0.2.1"], lock)
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
    assert Dict.equal? [ecto: "0.2.0", postgrex: "0.2.0", ex_doc: "0.0.1"], Explex.resolve(deps)
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
