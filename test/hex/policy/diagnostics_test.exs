defmodule Hex.Policy.DiagnosticsTest do
  use HexTest.Case
  alias Hex.Policy.Diagnostics

  defp policy(opts) do
    opts = Map.new(opts)
    {cooldown, opts} = Map.pop(opts, :cooldown)

    repositories =
      if cooldown do
        [%{repository: "hexpm", restriction: %{cooldown: cooldown}, overrides: []}]
      else
        []
      end

    Map.merge(
      %{repository: "myorg", visibility: :VISIBILITY_PUBLIC, repositories: repositories},
      opts
    )
  end

  describe "resolution_summary/3" do
    test "returns nil when no policy is loaded" do
      assert Diagnostics.resolution_summary(nil, [], "0d") == nil
    end

    test "returns a header + cooldown line + per-version listing" do
      p = policy(name: "strict-prod", cooldown: "14d")

      filtered = [
        %{
          repo: "hexpm",
          package: "phoenix",
          version: "1.7.18",
          reasons: [{:advisory, :SEVERITY_HIGH}]
        }
      ]

      out = Diagnostics.resolution_summary(p, filtered, "0d")
      assert out =~ "Active policy: myorg/strict-prod"
      assert out =~ "Effective cooldown: 14d (myorg/strict-prod)"
      assert out =~ "Policy hid 1 candidate version:\n"
      assert out =~ "  phoenix 1.7.18 — advisory ≥ HIGH"
    end

    test "pluralizes the hidden count and lists versions newest-first" do
      p = policy(name: "strict-prod")

      filtered = [
        %{repo: "hexpm", package: "phoenix", version: "1.7.18", reasons: [:override_deny]},
        %{repo: "hexpm", package: "phoenix", version: "1.7.19", reasons: [:override_deny]}
      ]

      out = Diagnostics.resolution_summary(p, filtered, "0d")
      assert out =~ "Policy hid 2 candidate versions:"

      [_count_line, line1, line2] =
        out |> String.split("\n", trim: true) |> Enum.take(-3)

      assert line1 =~ "phoenix 1.7.19"
      assert line2 =~ "phoenix 1.7.18"
    end

    test "caps the listing per package and points at mix hex.policy why" do
      p = policy(name: "strict-prod")

      filtered =
        for minor <- 0..7 do
          %{repo: "hexpm", package: "phoenix", version: "1.#{minor}.0", reasons: [:override_deny]}
        end

      out = Diagnostics.resolution_summary(p, filtered, "0d")
      assert out =~ "Policy hid 8 candidate versions:"
      assert out =~ "phoenix 1.7.0"
      assert out =~ "phoenix 1.3.0"
      refute out =~ "phoenix 1.2.0"
      assert out =~ "  ...and 3 more — run `mix hex.policy why phoenix`"
    end

    test "omits the cooldown line when the strictest cooldown is local" do
      p = policy(name: "strict-prod", cooldown: "7d")

      out = Diagnostics.resolution_summary(p, [], "1mo")
      assert out == "Active policy: myorg/strict-prod"
    end
  end

  describe "effective_cooldown/2" do
    test "is nil when nothing sets a cooldown" do
      assert Diagnostics.effective_cooldown(policy(name: "a"), "0d") == nil
    end

    test "picks the strictest across local and policy tabs" do
      assert {{"myorg", "b"}, "14d"} =
               Diagnostics.effective_cooldown(policy(name: "b", cooldown: "14d"), "2d")
    end

    test "local config can be the strictest" do
      assert {:local, "1mo"} =
               Diagnostics.effective_cooldown(policy(name: "a", cooldown: "7d"), "1mo")
    end

    test "handles a nil policy" do
      assert {:local, "7d"} = Diagnostics.effective_cooldown(nil, "7d")
    end
  end

  describe "failure_note/1" do
    test "returns nil when nothing filtered" do
      assert Diagnostics.failure_note([]) == nil
    end

    test "groups by package and lists reasons" do
      out =
        Diagnostics.failure_note([
          %{
            repo: "hexpm",
            package: "decimal",
            version: "2.0.0",
            reasons: [{:retirement, :RETIRED_SECURITY}]
          },
          %{
            repo: "hexpm",
            package: "decimal",
            version: "2.0.1",
            reasons: [{:advisory, :SEVERITY_HIGH}]
          }
        ])

      assert out =~ "Note: active policy hides 2 versions of \"decimal\""
      assert out =~ "decimal 2.0.0"
      assert out =~ "decimal 2.0.1"
      assert out =~ "advisory ≥ HIGH"
      assert out =~ "retirement: security"
    end

    test "formats cooldown and override reasons" do
      out =
        Diagnostics.failure_note([
          %{
            repo: "hexpm",
            package: "phoenix",
            version: "1.7.19",
            reasons: [{:cooldown, "14d", ~D[2026-05-20]}]
          },
          %{
            repo: "hexpm",
            package: "compromised",
            version: "1.0.0",
            reasons: [:override_deny]
          }
        ])

      assert out =~ "hides 1 version of \"phoenix\""
      assert out =~ "cooldown 14d; eligible 2026-05-20"
      assert out =~ "override deny"
    end

    test "caps the per-package listing like the resolution summary" do
      filtered =
        for minor <- 0..6 do
          %{repo: "hexpm", package: "phoenix", version: "1.#{minor}.0", reasons: [:override_deny]}
        end

      out = Diagnostics.failure_note(filtered)
      assert out =~ "hides 7 versions of \"phoenix\""
      assert out =~ "phoenix 1.6.0"
      assert out =~ "phoenix 1.2.0"
      refute out =~ "phoenix 1.1.0"
      assert out =~ "  ...and 2 more — run `mix hex.policy why phoenix`"
    end
  end
end
