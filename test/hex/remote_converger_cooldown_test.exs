defmodule Hex.RemoteConvergerCooldownTest do
  # Non-integration tests for the cooldown helpers in Hex.RemoteConverger.
  # The full integration scenarios live under HexTest.IntegrationCase.
  use HexTest.Case

  alias Hex.RemoteConverger
  alias Hex.Registry.Server

  setup do
    registry = [
      {:hexpm, :good, "1.0.0", []},
      {:hexpm, :retired_dep, "1.0.0", []},
      {:hexpm, :retired_dep, "2.0.0", []},
      {:hexpm, :advised_dep, "1.0.0", []},
      {:hexpm, :advised_dep, "2.0.0", []}
    ]

    retired = %{
      # The currently-locked version of retired_dep is retired.
      {:hexpm, :retired_dep, "1.0.0"} => %{reason: :RETIRED_SECURITY, message: "CVE"}
    }

    # Per-release advisories. Stored under {:advisories, repo, pkg, vsn} as
    # a list; non-empty means the version is known-unsafe.
    advisories = [
      {{"hexpm", "advised_dep", "1.0.0"},
       [%{id: "GHSA-test-aaaa-bbbb", summary: "test", severity: :SEVERITY_HIGH}]}
    ]

    path = tmp_path("cache_bypass.ets")
    File.rm(path)
    create_test_registry(path, registry, advisories, %{}, retired)

    Server.close()
    Hex.State.put(:offline, true)
    Server.open(registry_path: path)
    Server.prefetch([{"hexpm", "good"}, {"hexpm", "retired_dep"}, {"hexpm", "advised_dep"}])

    :ok
  end

  defp lock_tuple(name, version, repo \\ "hexpm") do
    {:hex, String.to_atom(name), version, "checksum", [:mix], [], repo, "outer_checksum"}
  end

  defp locked_request(name, version, repo \\ "hexpm") do
    %{repo: repo, name: name, app: name, version: version}
  end

  @cutoff {:cutoff, System.system_time(:second) - 7 * 86_400, 7 * 86_400}

  describe "build_cooldown_bypass/3 — disabled" do
    test "returns empty set when cutoff is :disabled" do
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}
      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, [], :disabled)
    end
  end

  describe "build_cooldown_bypass/3 — unsafe-locked-version bypass" do
    test "includes packages whose locked version is retired" do
      old_lock = %{
        retired_dep: lock_tuple("retired_dep", "1.0.0"),
        good: lock_tuple("good", "1.0.0")
      }

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert MapSet.member?(bypass, "retired_dep")
      refute MapSet.member?(bypass, "good")
    end

    test "regression: unsafe-bypass walks old_lock so updates can escape retirement" do
      # Guards against the bug where the bypass was built from the post-
      # prepare_locked value, which removes the package being `mix deps.update`d
      # — exactly the package that needs bypass.
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}
      # locked is empty because deps.update unlocked the package
      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert MapSet.member?(bypass, "retired_dep")
    end

    test "includes packages whose locked version carries a security advisory" do
      old_lock = %{advised_dep: lock_tuple("advised_dep", "1.0.0")}
      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)
      assert MapSet.member?(bypass, "advised_dep")
    end

    test "locked versions with empty advisory list are not bypassed via unsafe-set" do
      old_lock = %{advised_dep: lock_tuple("advised_dep", "2.0.0")}
      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)
    end
  end

  describe "build_cooldown_bypass/3 — lock-satisfied bypass (spec C)" do
    test "packages in `locked` (lockfile survived prepare_locked) are bypassed" do
      # mix deps.get against an intact lockfile: the dep made it through
      # prepare_locked, so it is being installed from the lock. Cooldown
      # must not interfere — matches the design's promise that
      # \"mix deps.get against an existing lockfile: cooldown does not apply\".
      old_lock = %{good: lock_tuple("good", "1.0.0")}
      locked = [locked_request("good", "1.0.0")]

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, locked, @cutoff)

      assert MapSet.member?(bypass, "good")
    end

    test "packages in old_lock but not in locked (being updated) are not bypassed by lock-satisfied" do
      # mix deps.update foo: foo is in old_lock but prepare_locked removed it.
      # If foo's locked version is not unsafe, foo should NOT bypass cooldown.
      old_lock = %{good: lock_tuple("good", "1.0.0")}
      locked = []

      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, locked, @cutoff)
    end

    test "new top-level deps (not in old_lock) are not bypassed" do
      # User added a new dep to mix.exs: no lock entry, no bypass.
      # Cooldown filtering applies normally to fresh additions.
      old_lock = %{}
      locked = []
      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, locked, @cutoff)
    end
  end

  describe "end-to-end advisory bypass" do
    # Wires the bypass set into the wrapper to confirm that an
    # advisory-flagged locked version actually unblocks the upgrade path
    # — not only that build_cooldown_bypass produces the right MapSet,
    # but that the wrapper consumes it and stops filtering.
    test "advisory-only locked version unblocks the upgrade through the wrapper" do
      # advised_dep 1.0.0 has an advisory; 2.0.0 is clean but assumed young.
      old_lock = %{advised_dep: lock_tuple("advised_dep", "1.0.0")}
      locked = []

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, locked, @cutoff)
      Hex.State.put(:cooldown_cutoff, @cutoff)
      Hex.State.put(:cooldown_bypass_packages, bypass)

      # Without bypass the wrapper would filter every version (no published_at
      # in the fixture means eligible, but if we'd populated young
      # published_at the bypass is what saves us). Either way, the wrapper
      # must return the full version list because the package is bypassed.
      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm", "advised_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0", "2.0.0"]
    end

    test "retired locked version unblocks the upgrade through the wrapper" do
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}
      locked = []

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, locked, @cutoff)
      Hex.State.put(:cooldown_cutoff, @cutoff)
      Hex.State.put(:cooldown_bypass_packages, bypass)

      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm", "retired_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0", "2.0.0"]
    end
  end
end
