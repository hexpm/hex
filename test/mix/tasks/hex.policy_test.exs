defmodule Mix.Tasks.Hex.PolicyTest do
  use HexTest.Case, async: false
  import ExUnit.CaptureIO

  setup do
    Hex.State.put(:active_policy, nil)
    Mix.shell(Mix.Shell.IO)
    on_exit(fn -> Mix.shell(Hex.Shell.Process) end)
    :ok
  end

  describe "show" do
    test "prints 'no active policy' message when empty" do
      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ "No active policy"
    end

    test "prints the active policy with key fields" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{
            repository: "hexpm",
            restriction: %{
              cooldown: "14d",
              advisory_min_severity: :SEVERITY_HIGH,
              retirement_reasons: [:RETIRED_INVALID, :RETIRED_SECURITY]
            },
            overrides: []
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ "Active policy"
      assert out =~ "myorg/strict-prod"
      assert out =~ "public"
      assert out =~ "14d"
      assert out =~ "HIGH"
    end

    test "lists each package override with its action" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{
            repository: "hexpm",
            restriction: %{},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ALLOW,
                ref: %{package: "plug", requirement: ">= 1.0.0"},
                comment: "Reviewed release line"
              },
              %{action: :OVERRIDE_ACTION_DENY, ref: %{package: "evil_dep"}}
            ]
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ "plug"
      assert out =~ ">= 1.0.0"
      assert out =~ "ALLOW"
      assert out =~ "Reviewed release line"
      assert out =~ "evil_dep"
      assert out =~ "DENY"
    end

    test "renders a (none) placeholder when a repository has no overrides" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{repository: "hexpm", restriction: %{}, overrides: []}
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ "Overrides:"
      assert out =~ "(none)"
    end

    test "lists advisory, retirement, and cooldown overrides with optional comments" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{
            repository: "hexpm",
            restriction: %{},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug", requirement: "~> 1.18"},
                advisory_id: "CVE-2026-12345",
                comment: "The affected parser is disabled"
              },
              %{
                action: :OVERRIDE_ACTION_RETIREMENT,
                ref: %{package: "legacy"},
                retirement_reason: :RETIRED_DEPRECATED
              },
              %{
                action: :OVERRIDE_ACTION_COOLDOWN,
                ref: %{package: "hotfix", requirement: "== 2.0.1"},
                comment: "Urgent fix"
              }
            ]
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ "Overrides:"
      assert out =~ "plug ~> 1.18"
      assert out =~ "ADVISORY CVE-2026-12345"
      assert out =~ "The affected parser is disabled"
      assert out =~ "legacy"
      assert out =~ "RETIREMENT deprecated"
      assert out =~ "hotfix == 2.0.1"
      assert out =~ "COOLDOWN"
      assert out =~ "Urgent fix"
    end

    test "reports malformed overrides without rendering their fields" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{
            repository: "hexpm",
            restriction: %{},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug"},
                advisory_id: "CVE-2026-12345",
                comment: "bad\nline"
              },
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug"},
                advisory_id: "CVE-2026-12345",
                comment: "bidi\u202Etext"
              },
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: <<255>>},
                advisory_id: "CVE-2026-12345"
              },
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug\nspoof"},
                advisory_id: "CVE-2026-12345"
              },
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug\u202Espoof"},
                advisory_id: "CVE-2026-12345"
              },
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug"},
                advisory_id: "CVE-2026-12345\nspoof"
              }
            ]
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert length(:binary.matches(out, "(invalid override ignored)")) == 6
      refute out =~ "bad"
      refute out =~ "bidi"
      refute out =~ "spoof"
    end

    test "identifies unsupported override actions and their scope" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{
            repository: "hexpm",
            restriction: %{},
            overrides: [
              %{
                action: 99,
                ref: %{package: "decimal", requirement: "== 1.0.0"}
              }
            ]
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ ~s(package "decimal" requirement "== 1.0.0")
      assert out =~ "UNKNOWN ACTION 99"
      assert out =~ "(ignored)"
    end
  end

  describe "why" do
    test "a bare invocation (no subcommand) raises the usage message" do
      assert_raise Mix.Error, ~r/Invalid arguments, expected one of:/, fn ->
        Mix.Tasks.Hex.Policy.run([])
      end
    end

    test "complains when package name is missing" do
      assert_raise Mix.Error, ~r/Invalid arguments, expected one of:/, fn ->
        Mix.Tasks.Hex.Policy.run(["why"])
      end
    end

    test "raises a short usage message on unknown arguments" do
      assert_raise Mix.Error, ~r/Invalid arguments, expected one of:/, fn ->
        Mix.Tasks.Hex.Policy.run(["bogus"])
      end
    end

    test "rejects empty halves like myorg/ or /pkg" do
      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC
      })

      assert_raise Mix.Error, ~r/Invalid package argument/, fn ->
        Mix.Tasks.Hex.Policy.run(["why", "myorg/"])
      end

      assert_raise Mix.Error, ~r/Invalid package argument/, fn ->
        Mix.Tasks.Hex.Policy.run(["why", "/foo"])
      end
    end

    test "shows scoped override comments while preserving other blocking findings" do
      path = tmp_path("policy_why_registry.ets")
      File.rm(path)

      advisories = [
        {{"hexpm", "why_pkg", "1.0.0"},
         [
           %{
             id: "GHSA-why-1",
             aliases: ["CVE-2026-7000"],
             severity: :SEVERITY_HIGH
           },
           %{id: "GHSA-why-2", aliases: [], severity: :SEVERITY_HIGH}
         ]}
      ]

      create_test_registry(
        path,
        [{:hexpm, :why_pkg, "1.0.0", []}],
        advisories,
        %{},
        %{}
      )

      Hex.Registry.Server.close()
      Hex.State.put(:offline, true)

      Hex.State.put(:active_policy, %{
        repository: "myorg",
        name: "strict-prod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [
          %{
            repository: "hexpm",
            restriction: %{advisory_min_severity: :SEVERITY_LOW},
            overrides: [
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "why_pkg"},
                advisory_id: "CVE-2026-7000",
                comment: "The vulnerable path is disabled"
              }
            ]
          }
        ]
      })

      Hex.Registry.Server.open(registry_path: path)

      try do
        output = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["why", "why_pkg"]) end)
        assert output =~ "BLOCKED"
        assert output =~ "advisory CVE-2026-7000 accepted"
        assert output =~ "The vulnerable path is disabled"
      after
        Hex.Registry.Server.close()
      end
    end
  end
end
