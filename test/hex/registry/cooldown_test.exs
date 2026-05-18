defmodule Hex.Registry.CooldownTest do
  use HexTest.Case
  alias Hex.Registry.Cooldown
  alias Hex.Registry.Server

  setup do
    now = System.system_time(:second)

    registry = [
      {:hexpm, :foo, "1.0.0", []},
      {:hexpm, :foo, "1.1.0", []},
      {:hexpm, :foo, "1.2.0", []}
    ]

    publish_times = %{
      {:hexpm, :foo, "1.0.0"} => now - 30 * 86_400,
      {:hexpm, :foo, "1.1.0"} => now - 14 * 86_400,
      {:hexpm, :foo, "1.2.0"} => now - 2 * 86_400
    }

    path = tmp_path("cache.ets")
    File.rm(path)
    create_test_registry(path, registry, [], publish_times)

    Hex.State.put(:offline, true)
    Server.open(registry_path: path)
    Server.prefetch([{"hexpm", "foo"}])

    %{now: now}
  end

  test "passes through versions when cooldown disabled" do
    Hex.State.put(:cooldown_cutoff, :disabled)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new())

    {:ok, versions} = Cooldown.versions("hexpm", "foo")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0", "1.2.0"]
  end

  test "filters versions within cooldown window" do
    Hex.State.put(:cooldown, "7d")
    cutoff = Hex.Cooldown.build_cutoff()
    Hex.State.put(:cooldown_cutoff, cutoff)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new())

    {:ok, versions} = Cooldown.versions("hexpm", "foo")
    # 1.2.0 published 2 days ago should be filtered
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0"]
  end

  test "bypass set short-circuits filtering for a package" do
    Hex.State.put(:cooldown, "60d")
    cutoff = Hex.Cooldown.build_cutoff()
    Hex.State.put(:cooldown_cutoff, cutoff)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new(["foo"]))

    {:ok, versions} = Cooldown.versions("hexpm", "foo")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0", "1.2.0"]
  end

  test "cooldown_exclude_repos short-circuits filtering for repos in the list" do
    Hex.State.put(:cooldown, "60d")
    cutoff = Hex.Cooldown.build_cutoff()
    Hex.State.put(:cooldown_cutoff, cutoff)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new())
    Hex.State.put(:cooldown_exclude_repos, ["hexpm"])

    {:ok, versions} = Cooldown.versions("hexpm", "foo")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0", "1.2.0"]
  after
    Hex.State.put(:cooldown_exclude_repos, [])
  end

  test "cooldown_exclude_repos does not affect non-excluded repos" do
    Hex.State.put(:cooldown, "60d")
    cutoff = Hex.Cooldown.build_cutoff()
    Hex.State.put(:cooldown_cutoff, cutoff)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new())
    Hex.State.put(:cooldown_exclude_repos, ["hexpm:other"])

    # hexpm is not in the exclude list — cooldown still applies, filtering
    # everything in the fixture (all versions are <60d old).
    {:ok, versions} = Cooldown.versions("hexpm", "foo")
    assert versions == []
  after
    Hex.State.put(:cooldown_exclude_repos, [])
  end

  test "nil published_at is treated as eligible" do
    now = System.system_time(:second)

    registry = [
      {:hexpm, :legacy, "1.0.0", []},
      {:hexpm, :legacy, "1.1.0", []}
    ]

    publish_times = %{
      {:hexpm, :legacy, "1.1.0"} => now - 1 * 86_400
    }

    path = tmp_path("cache_legacy.ets")
    File.rm(path)
    create_test_registry(path, registry, [], publish_times)

    Server.close()
    Hex.State.put(:offline, true)
    Server.open(registry_path: path)
    Server.prefetch([{"hexpm", "legacy"}])

    Hex.State.put(:cooldown, "30d")
    cutoff = Hex.Cooldown.build_cutoff()
    Hex.State.put(:cooldown_cutoff, cutoff)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new())

    {:ok, versions} = Cooldown.versions("hexpm", "legacy")
    # 1.0.0 has nil published_at -> eligible; 1.1.0 is too fresh -> filtered
    assert Enum.map(versions, &to_string/1) == ["1.0.0"]
  end

  test "delegates prefetch and dependencies to Server" do
    Code.ensure_loaded!(Cooldown)
    assert function_exported?(Cooldown, :prefetch, 1)
    assert function_exported?(Cooldown, :dependencies, 3)
  end
end
