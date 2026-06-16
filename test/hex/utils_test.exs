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
          %{id: "CVE-2026-0001", url: :undefined},
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
      advisory = Map.put(@advisory, :aliases, [%{id: "CVE-2026-0001", url: :undefined}])

      assert render(advisory, "    ") ==
               "GHSA-test-0001 (HIGH)\n    aka: CVE-2026-0001\n    Remote code execution via crafted input\n    https://github.com/advisories/GHSA-test-0001"
    end
  end

  describe "escape_terminal/1" do
    test "keeps printable text, newlines and tabs" do
      assert Hex.Utils.escape_terminal("hello world") == "hello world"
      assert Hex.Utils.escape_terminal("café ☃") == "café ☃"
      assert Hex.Utils.escape_terminal("line1\nline2\tend") == "line1\nline2\tend"
    end

    test "escapes ANSI escapes and other control characters" do
      assert Hex.Utils.escape_terminal("\e[31mred\e[0m") == "\\e[31mred\\e[0m"
      assert Hex.Utils.escape_terminal("a\rb\bc\ad") == "a\\rb\\bc\\ad"
      assert Hex.Utils.escape_terminal(<<0x01, 0x1F>>) == "\\x01\\x1F"
      assert Hex.Utils.escape_terminal(<<0x7F>>) == "\\x7F"
    end

    test "escapes bidirectional overrides and C1 control characters" do
      assert Hex.Utils.escape_terminal("a\u{202E}b") == "a\\u{202E}b"
      assert Hex.Utils.escape_terminal("\u{2066}") == "\\u{2066}"
      assert Hex.Utils.escape_terminal(<<0x85::utf8>>) == "\\u{85}"
    end
  end

  defp render(advisory, line_prefix \\ "") do
    advisory
    |> Hex.Utils.format_advisory_ansi(line_prefix)
    |> IO.ANSI.format(false)
    |> IO.iodata_to_binary()
  end
end
