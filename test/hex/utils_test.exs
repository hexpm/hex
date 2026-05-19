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

  describe "format_advisory_ansi/2" do
    test "includes id, severity and url" do
      assert render(@advisory) ==
               "GHSA-test-0001 (HIGH)\nRemote code execution via crafted input\nhttps://github.com/advisories/GHSA-test-0001"
    end

    test "without severity omits severity" do
      advisory = Map.delete(@advisory, :severity)

      assert render(advisory) ==
               "GHSA-test-0001\nRemote code execution via crafted input\nhttps://github.com/advisories/GHSA-test-0001"
    end

    test "without url omits url" do
      advisory = Map.delete(@advisory, :html_url)

      assert render(advisory) ==
               "GHSA-test-0001 (HIGH)\nRemote code execution via crafted input"
    end

    test "applies line prefix to wrapped lines" do
      assert render(@advisory, "    ") ==
               "GHSA-test-0001 (HIGH)\n    Remote code execution via crafted input\n    https://github.com/advisories/GHSA-test-0001"
    end

    test "emits ANSI color for severity when enabled" do
      iodata = Hex.Utils.format_advisory_ansi(@advisory)
      rendered = iodata |> IO.ANSI.format(true) |> IO.iodata_to_binary()
      assert rendered =~ "\e[31m"
      assert rendered =~ "(HIGH)"
    end

    test "renders aliases on an aka: line when present" do
      advisory =
        Map.put(@advisory, :aliases, [
          %{id: "CVE-2026-0001", url: nil},
          %{id: "GHSA-other-0002", url: "https://osv.dev/vulnerability/GHSA-other-0002"}
        ])

      assert render(advisory) ==
               "GHSA-test-0001 (HIGH)\naka: CVE-2026-0001, GHSA-other-0002\nRemote code execution via crafted input\nhttps://github.com/advisories/GHSA-test-0001"
    end

    test "omits aka: line when aliases are empty" do
      advisory = Map.put(@advisory, :aliases, [])

      assert render(advisory) ==
               "GHSA-test-0001 (HIGH)\nRemote code execution via crafted input\nhttps://github.com/advisories/GHSA-test-0001"
    end

    test "aka: line respects line prefix" do
      advisory = Map.put(@advisory, :aliases, [%{id: "CVE-2026-0001", url: nil}])

      assert render(advisory, "    ") ==
               "GHSA-test-0001 (HIGH)\n    aka: CVE-2026-0001\n    Remote code execution via crafted input\n    https://github.com/advisories/GHSA-test-0001"
    end
  end

  defp render(advisory, line_prefix \\ "") do
    advisory
    |> Hex.Utils.format_advisory_ansi(line_prefix)
    |> IO.ANSI.format(false)
    |> IO.iodata_to_binary()
  end
end
