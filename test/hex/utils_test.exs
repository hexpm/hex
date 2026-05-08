defmodule Hex.UtilsTest do
  use HexTest.Case

  @advisory %{
    id: "GHSA-test-0001",
    summary: "Remote code execution via crafted input",
    html_url: "https://github.com/advisories/GHSA-test-0001",
    severity: :SEVERITY_HIGH,
    api_url: "https://hex.pm/api/advisories/GHSA-test-0001"
  }

  describe "advisory_severity/1" do
    test "maps all severity atoms to strings" do
      assert Hex.Utils.advisory_severity(:SEVERITY_NONE) == "NONE"
      assert Hex.Utils.advisory_severity(:SEVERITY_LOW) == "LOW"
      assert Hex.Utils.advisory_severity(:SEVERITY_MEDIUM) == "MEDIUM"
      assert Hex.Utils.advisory_severity(:SEVERITY_HIGH) == "HIGH"
      assert Hex.Utils.advisory_severity(:SEVERITY_CRITICAL) == "CRITICAL"
    end

    test "passes unknown values through" do
      assert Hex.Utils.advisory_severity("unknown") == "unknown"
    end
  end

  describe "format_advisory/1" do
    test "includes severity and url" do
      assert Hex.Utils.format_advisory(@advisory) ==
               "(HIGH) Remote code execution via crafted input - https://github.com/advisories/GHSA-test-0001"
    end

    test "without severity omits severity prefix" do
      advisory = Map.delete(@advisory, :severity)

      assert Hex.Utils.format_advisory(advisory) ==
               "Remote code execution via crafted input - https://github.com/advisories/GHSA-test-0001"
    end

    test "without url omits url suffix" do
      advisory = Map.delete(@advisory, :html_url)

      assert Hex.Utils.format_advisory(advisory) ==
               "(HIGH) Remote code execution via crafted input"
    end
  end
end
