defmodule Hex.IgnoresTest do
  use ExUnit.Case, async: true

  alias Hex.Ignores

  describe "parse_advisories/1" do
    test "nil, empty string and empty list parse to no entries" do
      assert Ignores.parse_advisories(nil) == {:ok, []}
      assert Ignores.parse_advisories("") == {:ok, []}
      assert Ignores.parse_advisories([]) == {:ok, []}
    end

    test "accepts a list of id strings" do
      assert Ignores.parse_advisories(["CVE-2026-32686", "GHSA-xxxx-yyyy-zzzz"]) ==
               {:ok, ["CVE-2026-32686", "GHSA-xxxx-yyyy-zzzz"]}
    end

    test "rejects non-string and empty entries" do
      assert Ignores.parse_advisories([:cve]) == :error
      assert Ignores.parse_advisories(["CVE-2026-32686", 1]) == :error
      assert Ignores.parse_advisories([""]) == :error
      assert Ignores.parse_advisories(%{}) == :error
    end

    test "parses a comma-separated environment string" do
      assert Ignores.parse_advisories("CVE-2026-32686, GHSA-xxxx-yyyy-zzzz ,") ==
               {:ok, ["CVE-2026-32686", "GHSA-xxxx-yyyy-zzzz"]}
    end
  end

  describe "parse_retirements/1" do
    test "nil, empty string and empty list parse to no entries" do
      assert Ignores.parse_retirements(nil) == {:ok, []}
      assert Ignores.parse_retirements("") == {:ok, []}
      assert Ignores.parse_retirements([]) == {:ok, []}
    end

    test "accepts atoms and atom-version pairs" do
      assert Ignores.parse_retirements([:decimal, phoenix: "1.0.0"]) ==
               {:ok, [{"decimal", nil}, {"phoenix", "1.0.0"}]}
    end

    test "rejects string names" do
      assert Ignores.parse_retirements(["decimal"]) == :error
      assert Ignores.parse_retirements([{"phoenix", "1.0.0"}]) == :error
    end

    test "rejects invalid versions and terms" do
      assert Ignores.parse_retirements(phoenix: "1.0") == :error
      assert Ignores.parse_retirements(phoenix: :latest) == :error
      assert Ignores.parse_retirements([nil]) == :error
      assert Ignores.parse_retirements("phoenix@1.0") == :error
      assert Ignores.parse_retirements(%{}) == :error
    end

    test "parses a comma-separated environment string" do
      assert Ignores.parse_retirements("decimal, phoenix@1.0.0") ==
               {:ok, [{"decimal", nil}, {"phoenix", "1.0.0"}]}
    end
  end

  describe "advisory matching" do
    @advisory %{
      id: "GHSA-xxxx-yyyy-zzzz",
      aliases: ["CVE-2026-32686"],
      summary: "Remote code execution",
      severity: :SEVERITY_HIGH
    }

    test "matches the advisory id case-insensitively" do
      assert Ignores.advisory_matches?(@advisory, "GHSA-xxxx-yyyy-zzzz")
      assert Ignores.advisory_matches?(@advisory, "ghsa-XXXX-yyyy-zzzz")
      refute Ignores.advisory_matches?(@advisory, "GHSA-aaaa-bbbb-cccc")
    end

    test "matches aliases case-insensitively" do
      assert Ignores.advisory_matches?(@advisory, "CVE-2026-32686")
      assert Ignores.advisory_matches?(@advisory, "cve-2026-32686")
    end

    test "handles advisories without aliases" do
      refute Ignores.advisory_matches?(%{id: "GHSA-1111-2222-3333"}, "CVE-2026-1")
      assert Ignores.advisory_matches?(%{id: "GHSA-1111-2222-3333"}, "GHSA-1111-2222-3333")
    end

    test "advisory_ignored? checks all configured ids" do
      assert Ignores.advisory_ignored?(@advisory, ["CVE-2020-0000", "CVE-2026-32686"])
      refute Ignores.advisory_ignored?(@advisory, ["CVE-2020-0000"])
      refute Ignores.advisory_ignored?(@advisory, [])
    end
  end

  describe "retirement matching" do
    test "name-only entries match any version" do
      assert Ignores.retirement_matches?("decimal", "1.0.0", {"decimal", nil})
      assert Ignores.retirement_matches?("decimal", "2.0.0", {"decimal", nil})
      refute Ignores.retirement_matches?("phoenix", "1.0.0", {"decimal", nil})
    end

    test "pinned entries match exactly one version" do
      assert Ignores.retirement_matches?("phoenix", "1.0.0", {"phoenix", "1.0.0"})
      refute Ignores.retirement_matches?("phoenix", "1.0.1", {"phoenix", "1.0.0"})
      refute Ignores.retirement_matches?("decimal", "1.0.0", {"phoenix", "1.0.0"})
    end

    test "retirement_ignored? checks all configured entries" do
      entries = [{"decimal", nil}, {"phoenix", "1.0.0"}]
      assert Ignores.retirement_ignored?("decimal", "3.0.0", entries)
      assert Ignores.retirement_ignored?("phoenix", "1.0.0", entries)
      refute Ignores.retirement_ignored?("phoenix", "1.1.0", entries)
      refute Ignores.retirement_ignored?("ecto", "1.0.0", [])
    end
  end

  describe "split_advisories/2" do
    @eef %{
      id: "EEF-0001",
      aliases: ["CVE-2026-33333"],
      summary: "Remote code execution",
      severity: :SEVERITY_HIGH
    }
    @ghsa %{
      id: "GHSA-aaaa-bbbb-cccc",
      aliases: ["CVE-2026-33333"],
      summary: "Remote code execution",
      severity: :SEVERITY_HIGH
    }
    @other %{id: "GHSA-dddd-eeee-ffff", summary: "Denial of service", severity: :SEVERITY_LOW}

    test "no ignores keeps everything active" do
      assert Ignores.split_advisories([@eef, @ghsa, @other], []) == {[], [@eef, @ghsa, @other]}
    end

    test "ignoring one member id suppresses the whole aliased group" do
      assert Ignores.split_advisories([@eef, @ghsa, @other], ["EEF-0001"]) ==
               {[@eef, @ghsa], [@other]}
    end

    test "ignoring the shared CVE suppresses the whole aliased group" do
      assert Ignores.split_advisories([@eef, @ghsa, @other], ["cve-2026-33333"]) ==
               {[@eef, @ghsa], [@other]}
    end

    test "ungrouped advisories are matched individually" do
      assert Ignores.split_advisories([@other], ["GHSA-dddd-eeee-ffff"]) == {[@other], []}
    end
  end
end
