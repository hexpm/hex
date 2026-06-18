defmodule Hex.RemoteConvergerPolicyTest do
  use HexTest.Case
  import ExUnit.CaptureIO

  test "with no policy configured, Hex.Policy.load returns nil" do
    in_tmp("remote_converger_no_policy", fn ->
      Hex.State.put(:config_home, File.cwd!())
      original = System.get_env("HEX_POLICY")
      System.delete_env("HEX_POLICY")

      try do
        Hex.State.refresh()
        assert {:ok, nil} = Hex.Policy.load()
      after
        case original do
          nil -> System.delete_env("HEX_POLICY")
          value -> System.put_env("HEX_POLICY", value)
        end
      end
    end)
  end

  test "an empty HEX_POLICY disables a configured policy for the invocation" do
    in_tmp("remote_converger_empty_env_policy", fn ->
      # State.refresh/0 re-reads the config from inside the state agent,
      # which resolves the path from HEX_HOME rather than :config_home
      Hex.State.put(:config_home, File.cwd!())
      original_home = System.get_env("HEX_HOME")
      System.put_env("HEX_HOME", File.cwd!())
      original = System.get_env("HEX_POLICY")
      System.delete_env("HEX_POLICY")

      try do
        Hex.Config.update(policy: "hexpm:myorg/strict-prod")
        Hex.State.refresh()
        assert "hexpm:myorg/strict-prod" == Hex.State.fetch!(:policy)

        System.put_env("HEX_POLICY", "")
        Hex.State.refresh()
        assert Hex.State.fetch!(:policy) == nil
        assert {:ok, nil} = Hex.Policy.load()
      after
        case original do
          nil -> System.delete_env("HEX_POLICY")
          value -> System.put_env("HEX_POLICY", value)
        end

        case original_home do
          nil -> System.delete_env("HEX_HOME")
          value -> System.put_env("HEX_HOME", value)
        end
      end
    end)
  end

  test "a malformed HEX_POLICY fails resolution instead of degrading to unenforced" do
    in_tmp("remote_converger_malformed_policy", fn ->
      Hex.State.put(:config_home, File.cwd!())
      original = System.get_env("HEX_POLICY")
      System.put_env("HEX_POLICY", "myorgstrict")

      try do
        Hex.State.refresh()
        assert {:invalid, "myorgstrict"} = Hex.State.fetch!(:policy)

        assert_raise Mix.Error, ~r/Invalid policy configuration: "myorgstrict"/, fn ->
          Hex.Policy.load()
        end
      after
        case original do
          nil -> System.delete_env("HEX_POLICY")
          value -> System.put_env("HEX_POLICY", value)
        end
      end
    end)
  end

  test "print_policy_summary/0 prints the active policy block" do
    Mix.shell(Mix.Shell.IO)
    on_exit(fn -> Mix.shell(Hex.Shell.Process) end)

    Hex.State.put(:active_policy, %{
      repository: "myorg",
      name: "strict-prod",
      visibility: :VISIBILITY_PUBLIC,
      repositories: []
    })

    Hex.State.put(:policy_filtered_versions, [
      %{repo: "hexpm", package: "phoenix", version: "1.7.18", reasons: [:override_deny]}
    ])

    output = capture_io(fn -> Hex.RemoteConverger.print_policy_summary() end)
    assert output =~ "Active policy: myorg/strict-prod"
    assert output =~ "Policy hid 1 candidate version"
    assert output =~ "  phoenix 1.7.18 — override deny"
  end

  test "print_policy_summary/1 with list_filtered: false omits the hidden versions" do
    Mix.shell(Mix.Shell.IO)
    on_exit(fn -> Mix.shell(Hex.Shell.Process) end)

    Hex.State.put(:active_policy, %{
      repository: "myorg",
      name: "strict-prod",
      visibility: :VISIBILITY_PUBLIC,
      repositories: []
    })

    Hex.State.put(:policy_filtered_versions, [
      %{repo: "hexpm", package: "phoenix", version: "1.7.18", reasons: [:override_deny]}
    ])

    output =
      capture_io(fn -> Hex.RemoteConverger.print_policy_summary(list_filtered: false) end)

    assert output =~ "Active policy: myorg/strict-prod"
    refute output =~ "Policy hid"
    refute output =~ "phoenix 1.7.18"
  end

  test "print_policy_summary/0 prints nothing without an active policy" do
    Mix.shell(Mix.Shell.IO)
    on_exit(fn -> Mix.shell(Hex.Shell.Process) end)

    assert capture_io(fn -> Hex.RemoteConverger.print_policy_summary() end) == ""
  end
end
