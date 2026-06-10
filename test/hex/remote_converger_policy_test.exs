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
    assert output =~ "myorg/strict-prod: 1 (1 override deny)"
  end

  test "print_policy_summary/0 prints nothing without an active policy" do
    Mix.shell(Mix.Shell.IO)
    on_exit(fn -> Mix.shell(Hex.Shell.Process) end)

    assert capture_io(fn -> Hex.RemoteConverger.print_policy_summary() end) == ""
  end
end
