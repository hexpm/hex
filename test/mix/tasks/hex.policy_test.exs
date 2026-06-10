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

    test "default (no subcommand) is show" do
      out = capture_io(fn -> Mix.Tasks.Hex.Policy.run([]) end)
      assert out =~ "No active policy" || out =~ "Active policy"
    end
  end

  describe "why" do
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
