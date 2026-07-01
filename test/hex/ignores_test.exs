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
end
