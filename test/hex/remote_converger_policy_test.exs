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

  test "locked dependency warnings apply scoped overrides and keep unrelated advisories" do
    with_warning_registry(fn ->
      Hex.State.put(
        :active_policy,
        policy([
          %{
            action: :OVERRIDE_ACTION_ADVISORY,
            ref: %{package: "warning_pkg"},
            advisory_id: "CVE-2026-10001"
          },
          %{
            action: :OVERRIDE_ACTION_RETIREMENT,
            ref: %{package: "warning_pkg"},
            retirement_reason: :RETIRED_SECURITY
          }
        ])
      )

      dep = {"warning_pkg", "hexpm", nil, "1.0.0", nil}

      assert [new: [{^dep, nil, [%{id: "GHSA-warning-2"}]}]] =
               Hex.RemoteConverger.annotate_dependency_changes(new: [dep])
    end)
  end

  test "locked dependency warnings apply matching ALLOW overrides" do
    with_warning_registry(fn ->
      active_policy = policy([])
      [repo_policy] = active_policy.repositories

      active_policy = %{
        active_policy
        | repositories: [
            %{
              repo_policy
              | overrides: [
                  %{action: :OVERRIDE_ACTION_ALLOW, ref: %{package: "warning_pkg"}}
                ]
            }
          ]
      }

      Hex.State.put(:active_policy, active_policy)
      dep = {"warning_pkg", "hexpm", nil, "1.0.0", nil}

      assert [new: [{^dep, nil, []}]] =
               Hex.RemoteConverger.annotate_dependency_changes(new: [dep])
    end)
  end

  defp with_warning_registry(fun) do
    path = tmp_path("policy_warning_registry.ets")
    File.rm(path)

    advisories = [
      {{"hexpm", "warning_pkg", "1.0.0"},
       [
         %{
           id: "GHSA-warning-1",
           aliases: ["CVE-2026-10001"],
           severity: :SEVERITY_HIGH
         },
         %{id: "GHSA-warning-2", aliases: [], severity: :SEVERITY_HIGH}
       ]}
    ]

    retired = %{
      {:hexpm, :warning_pkg, "1.0.0"} => %{reason: :RETIRED_SECURITY}
    }

    create_test_registry(
      path,
      [{:hexpm, :warning_pkg, "1.0.0", []}],
      advisories,
      %{},
      retired
    )

    Hex.Registry.Server.close()
    Hex.State.put(:offline, true)
    Hex.State.put(:ignore_advisories, [])
    Hex.State.put(:ignore_retirements, [])
    Hex.Registry.Server.open(registry_path: path)
    Hex.Registry.Server.prefetch([{"hexpm", "warning_pkg"}])

    try do
      fun.()
    after
      Hex.Registry.Server.close()
    end
  end

  defp policy(overrides) do
    %{
      repository: "myorg",
      name: "strict-prod",
      visibility: :VISIBILITY_PUBLIC,
      repositories: [
        %{
          repository: "hexpm",
          restriction: %{
            advisory_min_severity: :SEVERITY_LOW,
            retirement_reasons: [:RETIRED_SECURITY]
          },
          overrides: overrides
        }
      ]
    }
  end
end
