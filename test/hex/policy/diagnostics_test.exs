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
    test "returns nil when no policies are loaded" do
      assert Diagnostics.resolution_summary([], [], "0d") == nil
    end

    test "returns a header + cooldown line + per-policy counts" do
      policies = [
        policy(name: "strict-prod", cooldown: "14d"),
        policy(name: "baseline")
      ]

      filtered = [
        %{
          repo: "hexpm",
          package: "phoenix",
          version: "1.7.18",
          blockers: [%{policy: hd(policies), reason: {:advisory, :SEVERITY_HIGH}}]
        }
      ]

      out = Diagnostics.resolution_summary(policies, filtered, "0d")
      assert out =~ "Active policies: myorg/baseline, myorg/strict-prod (2)"
      assert out =~ "Effective cooldown: 14d (myorg/strict-prod)"
      assert out =~ "Policies hid 1 candidate versions"
      assert out =~ "myorg/strict-prod: 1 (1 advisory)"
    end
  end

  describe "effective_cooldown/2" do
    test "is nil when nothing sets a cooldown" do
      assert Diagnostics.effective_cooldown([policy(name: "a")], "0d") == nil
    end

    test "picks the strictest across local and policy tabs" do
      policies = [policy(name: "a", cooldown: "7d"), policy(name: "b", cooldown: "14d")]
      assert {{"myorg", "b"}, "14d"} = Diagnostics.effective_cooldown(policies, "2d")
    end

    test "local config can be the strictest" do
      assert {:local, "1mo"} =
               Diagnostics.effective_cooldown([policy(name: "a", cooldown: "7d")], "1mo")
    end
  end

  describe "failure_note/1" do
    test "returns nil when nothing filtered" do
      assert Diagnostics.failure_note([]) == nil
    end

    test "groups by package and lists blockers" do
      pol = policy(name: "strict-prod")

      out =
        Diagnostics.failure_note([
          %{
            repo: "hexpm",
            package: "decimal",
            version: "2.0.0",
            blockers: [%{policy: pol, reason: {:retirement, :RETIRED_SECURITY}}]
          },
          %{
            repo: "hexpm",
            package: "decimal",
            version: "2.0.1",
            blockers: [%{policy: pol, reason: {:advisory, :SEVERITY_HIGH}}]
          }
        ])

      assert out =~ "Note: active policies hide 2 versions of \"decimal\""
      assert out =~ "decimal 2.0.0"
      assert out =~ "decimal 2.0.1"
      assert out =~ "advisory ≥ HIGH"
      assert out =~ "retirement: security"
    end

    test "formats cooldown and override reasons" do
      pol = policy(name: "strict-prod")

      out =
        Diagnostics.failure_note([
          %{
            repo: "hexpm",
            package: "phoenix",
            version: "1.7.19",
            blockers: [%{policy: pol, reason: {:cooldown, "14d", ~D[2026-05-20]}}]
          },
          %{
            repo: "hexpm",
            package: "compromised",
            version: "1.0.0",
            blockers: [%{policy: pol, reason: :override_deny}]
          }
        ])

      assert out =~ "cooldown 14d; eligible 2026-05-20"
      assert out =~ "override deny"
    end
  end

  describe "format_load_error/1" do
    test "invalid_policy_config" do
      assert Diagnostics.format_load_error(:invalid_policy_config) =~
               "Policy configuration is invalid"
    end

    test "fallback" do
      assert Diagnostics.format_load_error(:something) =~ "Policy loading failed"
    end
  end
end
