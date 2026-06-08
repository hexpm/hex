defmodule Hex.Registry.PolicyTest do
  use HexTest.Case
  alias Hex.Registry.Policy
  alias Hex.Registry.Server

  setup do
    registry = [
      {:hexpm, :clean, "1.0.0", []},
      {:hexpm, :clean, "1.1.0", []},
      {:hexpm, :advised, "1.0.0", []},
      {:hexpm, :advised, "1.1.0", []},
      {:hexpm, :retired, "1.0.0", []},
      {:hexpm, :retired, "1.1.0", []}
    ]

    advisories = [
      {{"hexpm", "advised", "1.0.0"},
       [%{id: "GHSA-test-aaaa-bbbb", summary: "test", severity: :SEVERITY_HIGH}]}
    ]

    retired = %{
      {:hexpm, :retired, "1.0.0"} => %{reason: :RETIRED_SECURITY, message: "CVE"}
    }

    path = tmp_path("cache_policy.ets")
    File.rm(path)
    create_test_registry(path, registry, advisories, %{}, retired)

    Server.close()
    Hex.State.put(:offline, true)
    Server.open(registry_path: path)
    Server.prefetch([{"hexpm", "clean"}, {"hexpm", "advised"}, {"hexpm", "retired"}])

    # Reset diagnostics for each test
    Hex.State.put(:policy_filtered_versions, [])
    Hex.State.put(:policies, %{})
    Hex.State.put(:policy_locked_versions, %{})

    # Disable cooldown so Hex.Registry.Cooldown is a no-op pass-through
    Hex.State.put(:cooldown_cutoff, :disabled)
    Hex.State.put(:cooldown_bypass_packages, MapSet.new())
    Hex.State.put(:cooldown_locked_versions, %{})
    Hex.State.put(:cooldown_filtered_versions, [])

    :ok
  end

  defp policy(name, tab_fields) do
    %{
      {"hexpm:myorg", name} => %{
        repository: "myorg",
        name: name,
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          Map.merge(%{repository: "hexpm", overrides: []}, Map.new(tab_fields))
        ]
      }
    }
  end

  test "passes through when no policies are configured" do
    assert {:ok, versions} = Policy.versions("hexpm", "clean")
    {:ok, expected} = Server.versions("hexpm", "clean")
    assert versions == expected
  end

  test "passes through when no policies are configured for a package with advisories" do
    assert {:ok, versions} = Policy.versions("hexpm", "advised")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0"]
    assert Hex.State.fetch!(:policy_filtered_versions) == []
  end

  test "filters versions that an advisory rule blocks" do
    Hex.State.put(
      :policies,
      policy("strict", restriction: %{advisory_min_severity: :SEVERITY_HIGH})
    )

    assert {:ok, versions} = Policy.versions("hexpm", "advised")
    assert Enum.map(versions, &to_string/1) == ["1.1.0"]

    [entry] = Hex.State.fetch!(:policy_filtered_versions)
    assert entry.repo == "hexpm"
    assert entry.package == "advised"
    assert entry.version == "1.0.0"

    assert [%{policy: %{name: "strict"}, reason: {:advisory, :SEVERITY_HIGH}}] =
             entry.blockers
  end

  test "filters versions that a retirement rule blocks" do
    Hex.State.put(
      :policies,
      policy("no-security-retired", restriction: %{retirement_reasons: [:RETIRED_SECURITY]})
    )

    assert {:ok, versions} = Policy.versions("hexpm", "retired")
    assert Enum.map(versions, &to_string/1) == ["1.1.0"]

    [entry] = Hex.State.fetch!(:policy_filtered_versions)
    assert entry.package == "retired"
    assert entry.version == "1.0.0"

    assert [
             %{policy: %{name: "no-security-retired"}, reason: {:retirement, :RETIRED_SECURITY}}
           ] = entry.blockers
  end

  test "a deny override blocks all versions of a package" do
    Hex.State.put(
      :policies,
      policy("blocklist",
        overrides: [%{action: :OVERRIDE_ACTION_DENY, ref: %{package: "clean"}}]
      )
    )

    assert {:ok, versions} = Policy.versions("hexpm", "clean")
    assert versions == []

    assert [%{version: "1.1.0"}, %{version: "1.0.0"}] =
             Hex.State.fetch!(:policy_filtered_versions)
  end

  test "an allow override bypasses the restriction" do
    Hex.State.put(
      :policies,
      policy("vetted",
        restriction: %{advisory_min_severity: :SEVERITY_HIGH},
        overrides: [%{action: :OVERRIDE_ACTION_ALLOW, ref: %{package: "advised"}}]
      )
    )

    assert {:ok, versions} = Policy.versions("hexpm", "advised")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0"]
    assert Hex.State.fetch!(:policy_filtered_versions) == []
  end

  test "a locked version is exempt from policy filtering" do
    Hex.State.put(
      :policies,
      policy("strict", restriction: %{advisory_min_severity: :SEVERITY_HIGH})
    )

    Hex.State.put(:policy_locked_versions, %{{"hexpm", "advised"} => ["1.0.0"]})

    assert {:ok, versions} = Policy.versions("hexpm", "advised")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0"]
    assert Hex.State.fetch!(:policy_filtered_versions) == []
  end

  test "no diagnostics recorded when nothing is blocked" do
    Hex.State.put(
      :policies,
      policy("strict", restriction: %{advisory_min_severity: :SEVERITY_HIGH})
    )

    assert {:ok, versions} = Policy.versions("hexpm", "clean")
    assert Enum.map(versions, &to_string/1) == ["1.0.0", "1.1.0"]
    assert Hex.State.fetch!(:policy_filtered_versions) == []
  end

  test "delegates prefetch and dependencies" do
    Code.ensure_loaded!(Policy)
    assert function_exported?(Policy, :prefetch, 1)
    assert function_exported?(Policy, :dependencies, 3)
  end
end
