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
      {:hexpm, :advised_dep, "2.0.0", []},
      {:"hexpm:myorg", :advised_dep, "1.0.0", []},
      {:"hexpm:myorg", :advised_dep, "2.0.0", []}
    ]

    retired = %{
      # The currently-locked version of retired_dep is retired.
      {:hexpm, :retired_dep, "1.0.0"} => %{reason: :RETIRED_SECURITY, message: "CVE"}
    }

    # Per-release advisories. Stored under {:advisories, repo, pkg, vsn} as
    # a list; non-empty means the version is known-unsafe.
    advisories = [
      {{"hexpm", "advised_dep", "1.0.0"},
       [%{id: "GHSA-test-aaaa-bbbb", summary: "test", severity: :SEVERITY_HIGH}]},
      {{"hexpm:myorg", "advised_dep", "1.0.0"},
       [%{id: "GHSA-test-cccc-dddd", summary: "test", severity: :SEVERITY_HIGH}]}
    ]

    # The 2.0.0 releases are inside the 7-day cooldown, so the wrapper only
    # returns them when the package is bypassed.
    publish_times = %{
      {:hexpm, :retired_dep, "2.0.0"} => System.system_time(:second) - 86_400,
      {:hexpm, :advised_dep, "2.0.0"} => System.system_time(:second) - 86_400,
      {:"hexpm:myorg", :advised_dep, "2.0.0"} => System.system_time(:second) - 86_400
    }

    path = tmp_path("cache_bypass.ets")
    File.rm(path)
    create_test_registry(path, registry, advisories, publish_times, retired)

    Server.close()
    Hex.State.put(:offline, true)
    Server.open(registry_path: path)

    Server.prefetch([
      {"hexpm", "good"},
      {"hexpm", "retired_dep"},
      {"hexpm", "advised_dep"},
      {"hexpm:myorg", "advised_dep"}
    ])

    :ok
  end

  defp lock_tuple(name, version, repo \\ "hexpm") do
    {:hex, String.to_atom(name), version, "checksum", [:mix], [], repo, "outer_checksum"}
  end

  defp lock_tuple_with_deps(name, version, child_names, repo \\ "hexpm") do
    deps =
      Enum.map(child_names, fn child ->
        {String.to_atom(child), "~> 0.0.0", [hex: child, repo: "hexpm", optional: false]}
      end)

    {:hex, String.to_atom(name), version, "checksum", [:mix], deps, repo, "outer_checksum"}
  end

  defp locked_request(name, version, repo \\ "hexpm") do
    %{repo: repo, name: name, app: name, version: version}
  end

  @cutoff {:cutoff, System.system_time(:second) - 7 * 86_400, 7 * 86_400}

  # Puts the cooldown state RemoteConverger.converge/2 sets up before resolving.
  defp put_cooldown_state(old_lock) do
    Hex.State.put(:cooldown_cutoff, @cutoff)

    Hex.State.put(
      :cooldown_bypass_packages,
      RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)
    )

    Hex.State.put(
      :cooldown_locked_versions,
      RemoteConverger.build_cooldown_locked_versions(old_lock)
    )

    Hex.State.put(:cooldown_filtered_versions, [])
  end

  describe "build_cooldown_bypass/3 — disabled" do
    test "returns empty set when cutoff is :disabled" do
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}
      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, [], :disabled)
    end
  end

  describe "build_cooldown_bypass/3 — unsafe-locked-version bypass" do
    test "excludes packages whose locked version is retired" do
      # Retiring a release takes the same package owner permission as
      # publishing one, so retirement can't lift the cooldown.
      old_lock = %{
        retired_dep: lock_tuple("retired_dep", "1.0.0"),
        good: lock_tuple("good", "1.0.0")
      }

      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)
    end

    test "regression: unsafe-bypass walks old_lock so updates can escape an advisory" do
      # Guards against the bug where the bypass was built from the post-
      # prepare_locked value, which removes the package being `mix deps.update`d
      # — exactly the package that needs bypass.
      old_lock = %{advised_dep: lock_tuple("advised_dep", "1.0.0")}
      # locked is empty because deps.update unlocked the package
      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert MapSet.member?(bypass, {"hexpm", "advised_dep"})
    end

    test "includes packages whose locked version carries a security advisory" do
      old_lock = %{
        advised_dep: lock_tuple("advised_dep", "1.0.0"),
        good: lock_tuple("good", "1.0.0")
      }

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert MapSet.member?(bypass, {"hexpm", "advised_dep"})
      refute MapSet.member?(bypass, {"hexpm", "good"})
    end

    test "entries are qualified by the locked repository" do
      old_lock = %{advised_dep: lock_tuple("advised_dep", "1.0.0")}
      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert MapSet.member?(bypass, {"hexpm", "advised_dep"})
      refute MapSet.member?(bypass, {"hexpm:myorg", "advised_dep"})
      refute MapSet.member?(bypass, "advised_dep")
    end

    test "locked versions with empty advisory list are not bypassed via unsafe-set" do
      old_lock = %{advised_dep: lock_tuple("advised_dep", "2.0.0")}
      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)
    end

    test "lock-children of unsafe parent are also bypassed" do
      # Escaping an unsafe parent typically requires re-resolving its
      # dependency subtree, and the new parent version usually wants newer
      # versions of those children. Mirror `mix deps.update`'s behavior of
      # unlocking the children alongside the explicit target.
      old_lock = %{
        advised_dep: lock_tuple_with_deps("advised_dep", "1.0.0", ["good", "retired_dep"]),
        good: lock_tuple("good", "1.0.0")
      }

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert MapSet.member?(bypass, {"hexpm", "advised_dep"})
      assert MapSet.member?(bypass, {"hexpm", "good"})
      assert MapSet.member?(bypass, {"hexpm", "retired_dep"})
    end

    test "children of retired parents are not bypassed" do
      old_lock = %{
        retired_dep: lock_tuple_with_deps("retired_dep", "1.0.0", ["good"])
      }

      assert MapSet.new() == RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)
    end

    test "children of safe parents are not bypassed" do
      old_lock = %{
        good: lock_tuple_with_deps("good", "1.0.0", ["advised_dep"])
      }

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

      assert MapSet.member?(bypass, {"hexpm", "good"})
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

  describe "print_cooldown_summary/1" do
    test "prints the summary when filtered versions were recorded" do
      now = System.system_time(:second)
      Hex.State.put(:cooldown, "7d")
      Hex.State.put(:cooldown_cutoff, Hex.Cooldown.build_cutoff())

      Hex.State.put(:cooldown_filtered_versions, [
        {"hexpm", "castore", "1.0.19", now - 6 * 86_400}
      ])

      RemoteConverger.print_cooldown_summary(%{})

      assert_received {:mix_shell, :info, [output]}
      assert output =~ "Versions filtered by cooldown:"
      assert output =~ "castore 1.0.19"
    end

    test "prints nothing when nothing was filtered" do
      Hex.State.put(:cooldown, "7d")
      Hex.State.put(:cooldown_cutoff, Hex.Cooldown.build_cutoff())
      Hex.State.put(:cooldown_filtered_versions, [])

      RemoteConverger.print_cooldown_summary(%{})

      refute_received {:mix_shell, :info, _}
    end

    test "prints nothing when cooldown is disabled" do
      Hex.State.put(:cooldown_cutoff, :disabled)
      Hex.State.put(:cooldown_filtered_versions, [])

      RemoteConverger.print_cooldown_summary(%{})

      refute_received {:mix_shell, :info, _}
    end

    test "tells how to update when the locked release is retired" do
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}
      put_cooldown_state(old_lock)

      # The resolver's lookup records the filtered 2.0.0 for the summary.
      {:ok, _versions} = Hex.Registry.Cooldown.versions("hexpm", "retired_dep")
      RemoteConverger.print_cooldown_summary(old_lock)

      assert_received {:mix_shell, :info, [output]}
      assert output =~ "retired_dep 2.0.0"

      assert output =~
               "Locked version 1.0.0 is retired. To update without the cooldown, " <>
                 "run `HEX_COOLDOWN=0 mix deps.update retired_dep`"
    end

    test "doesn't mention a retirement acknowledged with ignore_retirements" do
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}

      Hex.State.put(:ignore_retirements, [{"retired_dep", nil}])
      put_cooldown_state(old_lock)

      {:ok, _versions} = Hex.Registry.Cooldown.versions("hexpm", "retired_dep")
      RemoteConverger.print_cooldown_summary(old_lock)

      assert_received {:mix_shell, :info, [output]}
      assert output =~ "retired_dep 2.0.0"
      refute output =~ "Locked version"
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
      Hex.State.put(:cooldown_locked_versions, %{})
      Hex.State.put(:cooldown_filtered_versions, [])

      # Without bypass the wrapper would filter the young 2.0.0. The wrapper
      # must return the full version list because the package is bypassed.
      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm", "advised_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0", "2.0.0"]
    end

    test "advisory on an organization lock entry only lifts the cooldown in the locked repositories" do
      old_lock = %{
        advised_dep:
          {:hex, :advised_dep, "1.0.0", "checksum", [:mix],
           [
             {:good, "~> 1.0", [hex: "good", repo: "hexpm:myorg", optional: false]},
             {:retired_dep, "~> 1.0", [hex: "retired_dep", repo: "hexpm", optional: false]}
           ], "hexpm:myorg", "outer_checksum"}
      }

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, [], @cutoff)

      assert bypass ==
               MapSet.new([
                 {"hexpm:myorg", "advised_dep"},
                 {"hexpm:myorg", "good"},
                 {"hexpm", "retired_dep"}
               ])

      put_cooldown_state(old_lock)

      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm:myorg", "advised_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0", "2.0.0"]

      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm", "retired_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0", "2.0.0"]

      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm", "advised_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0"]
    end

    test "retired locked version doesn't unblock the upgrade through the wrapper" do
      old_lock = %{retired_dep: lock_tuple("retired_dep", "1.0.0")}
      locked = []

      bypass = RemoteConverger.build_cooldown_bypass(old_lock, locked, @cutoff)
      Hex.State.put(:cooldown_cutoff, @cutoff)
      Hex.State.put(:cooldown_bypass_packages, bypass)
      Hex.State.put(:cooldown_locked_versions, %{})
      Hex.State.put(:cooldown_filtered_versions, [])

      {:ok, versions} = Hex.Registry.Cooldown.versions("hexpm", "retired_dep")
      assert Enum.map(versions, &to_string/1) == ["1.0.0"]
    end
  end
end
