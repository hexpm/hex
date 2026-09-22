defmodule Hex.RemoteConvergerTest do
  use HexTest.IntegrationCase

  defmodule OutdatedDepsBefore.MixProject do
    def project do
      [
        app: :outdated_deps,
        version: "0.1.0",
        deps: [
          {:postgrex, "0.2.1", warn_if_outdated: true},
          {:ecto, "3.3.1", warn_if_outdated: true},
          {:ecto_sql, "3.3.2", warn_if_outdated: true}
        ]
      ]
    end
  end

  defmodule OutdatedDepsAfter.MixProject do
    def project do
      [
        app: :outdated_deps,
        version: "0.1.0",
        deps: [
          {:postgrex, ">= 0.0.0", warn_if_outdated: true},
          {:ecto, ">= 0.0.0", warn_if_outdated: true},
          {:ecto_sql, ">= 0.0.0", warn_if_outdated: true}
        ]
      ]
    end
  end

  test "deps with warn_if_outdated: true" do
    in_tmp(fn ->
      Mix.Project.push(OutdatedDepsBefore.MixProject)
      :ok = Mix.Tasks.Deps.Get.run([])

      Mix.Project.pop()
      Mix.Project.push(OutdatedDepsAfter.MixProject)

      output =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          :ok = Mix.Tasks.Deps.Get.run([])
        end)

      assert output =~ "ecto 3.3.2 is available"
      assert output =~ "ecto_sql 3.3.3 is available"
      refute output =~ "postgrex"
    end)
  end

  test "deps/2 returns valid requirements for registry dependencies" do
    Hex.Registry.Server.open()
    package = "unconstrained_parent"
    version = "1.0.0"

    :sys.replace_state(Hex.Registry.Server, fn %{ets: tid, fetched: fetched} = state ->
      :ets.insert(
        tid,
        {{:deps, "hexpm", package, version},
         [
           {"hexpm", "foo", "foo", ">= 0.5.50 or < 0.9.0", false},
           {"hexpm", "bar", "bar", "< 0.0.0-0", true}
         ]}
      )

      %{state | fetched: MapSet.put(fetched, {"hexpm", package})}
    end)

    lock = %{unconstrained_parent: {:hex, :unconstrained_parent, version}}
    deps = Hex.RemoteConverger.deps(%Mix.Dep{app: :unconstrained_parent}, lock)

    assert [
             {:foo, any_requirement, optional: false, hex: "foo", repo: nil},
             {:bar, empty_requirement, optional: true, hex: "bar", repo: nil}
           ] = deps

    assert any_requirement == ">= 0.0.0-0"
    assert {:ok, _requirement} = Version.parse_requirement(any_requirement)
    assert Hex.Solver.parse_constraint!(any_requirement) == %Hex.Solver.Constraints.Range{}

    assert empty_requirement == "< 0.0.0-0"
    assert {:ok, _requirement} = Version.parse_requirement(empty_requirement)
    assert Hex.Solver.parse_constraint!(empty_requirement) == %Hex.Solver.Constraints.Empty{}
  end

  defmodule WarnOutdatedWithHexOption.MixProject do
    def project do
      [
        app: :warn_outdated_with_hex_option,
        version: "0.1.0",
        deps: [
          # Package name is "package_name" but app name is "app_name"
          {:app_name, ">= 0.0.0", hex: :package_name, warn_if_outdated: true}
        ]
      ]
    end
  end

  defmodule PublicDepsWithExpiredOAuth.MixProject do
    def project do
      [
        app: :public_deps_with_expired_oauth,
        version: "0.1.0",
        deps: [
          {:postgrex, "0.2.1"}
        ]
      ]
    end
  end

  defmodule OrganizationDepsWithExpiredOAuth.MixProject do
    def project do
      [
        app: :organization_deps_with_expired_oauth,
        version: "0.1.0",
        deps: [
          {:private_prompt_pkg, "0.1.0", repo: "hexpm:remote_converger_org"}
        ]
      ]
    end
  end

  defp with_project(project, fun) do
    Mix.Project.push(project)

    try do
      fun.()
    after
      Mix.Project.pop()
    end
  end

  defp store_expired_oauth_token do
    Hex.OAuth.store_token(%{
      access_token: "expired_access_token",
      refresh_token: "invalid_refresh_token",
      expires_at: System.system_time(:second) - 3600
    })
  end

  defp new_repo_auth_user(prefix) do
    suffix = System.unique_integer([:positive])

    Hexpm.new_user(
      "#{prefix}_#{suffix}",
      "#{prefix}_#{suffix}@mail.com",
      "password",
      "#{prefix}_#{suffix}_key"
    )
  end

  test "deps with warn_if_outdated: true and hex: option" do
    in_tmp(fn ->
      Mix.Project.push(WarnOutdatedWithHexOption.MixProject)

      # This should not crash with KeyError when the package name differs from app name
      assert :ok = Mix.Tasks.Deps.Get.run([])
    end)
  end

  test "deps.get does not prompt for auth when only public deps are requested" do
    in_tmp(fn ->
      set_home_cwd()
      store_expired_oauth_token()

      with_project(PublicDepsWithExpiredOAuth.MixProject, fn ->
        assert :ok = Mix.Tasks.Deps.Get.run([])
      end)

      refute_received {:mix_shell, :yes?, _}

      # The unusable session is dropped after the first failed refresh, so the
      # rest of the resolution falls back to unauthenticated fetches instead of
      # retrying the doomed refresh for every package.
      assert Hex.State.get(:oauth_token) == nil
    end)
  end

  test "deps.get does not prompt when repo auth is already available" do
    in_tmp(fn ->
      set_home_cwd()

      auth = new_repo_auth_user("remote_converger_repo_auth_deps_get")

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, auth[:key])
      Hex.State.put(:repos, repos)

      store_expired_oauth_token()

      with_project(OrganizationDepsWithExpiredOAuth.MixProject, fn ->
        assert :ok = Mix.Tasks.Deps.Get.run([])
      end)

      refute_received {:mix_shell, :yes?, _}
    end)
  end

  @advisory %{
    id: "GHSA-rc-0001",
    summary: "Remote code execution via crafted input",
    html_url: "https://github.com/advisories/GHSA-rc-0001",
    severity: :SEVERITY_HIGH,
    api_url: "https://hex.pm/api/advisories/GHSA-rc-0001"
  }

  defmodule AdvisoryDeps.MixProject do
    def project do
      [
        app: :advisory_deps,
        version: "0.1.0",
        deps: [{:rc_advisory_package, "0.1.0"}]
      ]
    end
  end

  test "deps.get prints security advisory warning for resolved version" do
    auth =
      Hexpm.new_user(
        "rc_advisory_user",
        "rc_advisory@mail.com",
        "passpass",
        "rc_advisory_key"
      )

    Hexpm.new_package("hexpm", "rc_advisory_package", "0.1.0", [], %{}, auth)

    with_project(AdvisoryDeps.MixProject, fn ->
      in_tmp(fn ->
        Hex.State.put(:cache_home, tmp_path())
        Hex.State.put(:api_key, auth[:key])
        Mix.Dep.Lock.write(%{rc_advisory_package: {:hex, :rc_advisory_package, "0.1.0"}})

        :ok = Mix.Tasks.Deps.Get.run([])
        flush()

        :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
          :ets.insert(
            tid,
            {{:advisories, "hexpm", "rc_advisory_package", "0.1.0"}, [@advisory]}
          )

          state
        end)

        File.rm!("mix.lock")
        Mix.Task.clear()

        :ok = Mix.Tasks.Deps.Get.run([])

        info_messages = collect_info_messages([])

        assert Enum.any?(info_messages, fn msg ->
                 msg =~ "rc_advisory_package 0.1.0" and msg =~ "VULNERABLE!"
               end)

        assert Enum.any?(info_messages, fn msg ->
                 msg =~ "GHSA-rc-0001" and msg =~ "(HIGH)" and
                   msg =~ "Remote code execution via crafted input"
               end)
      end)
    end)
  end

  defp collect_info_messages(acc) do
    receive do
      {:mix_shell, :info, [msg]} -> collect_info_messages([msg | acc])
    after
      0 -> Enum.reverse(acc)
    end
  end

  defmodule IgnoredAdvisoryDeps.MixProject do
    def project do
      [
        app: :ignored_advisory_deps,
        version: "0.1.0",
        deps: [{:rc_ignored_package, "0.1.0"}]
      ]
    end
  end

  test "deps.get does not warn about ignored advisories" do
    auth =
      Hexpm.new_user(
        "rc_ignored_user",
        "rc_ignored@mail.com",
        "passpass",
        "rc_ignored_key"
      )

    Hexpm.new_package("hexpm", "rc_ignored_package", "0.1.0", [], %{}, auth)

    advisory = %{
      id: "GHSA-rc-0002",
      aliases: ["CVE-2026-22222"],
      summary: "Remote code execution via crafted input",
      html_url: "https://github.com/advisories/GHSA-rc-0002",
      severity: :SEVERITY_HIGH,
      api_url: "https://hex.pm/api/advisories/GHSA-rc-0002"
    }

    with_project(IgnoredAdvisoryDeps.MixProject, fn ->
      in_tmp(fn ->
        Hex.State.put(:cache_home, tmp_path())
        Hex.State.put(:api_key, auth[:key])
        Mix.Dep.Lock.write(%{rc_ignored_package: {:hex, :rc_ignored_package, "0.1.0"}})

        :ok = Mix.Tasks.Deps.Get.run([])
        flush()

        :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
          :ets.insert(
            tid,
            {{:advisories, "hexpm", "rc_ignored_package", "0.1.0"}, [advisory]}
          )

          state
        end)

        File.rm!("mix.lock")
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        assert output =~ "VULNERABLE!"
        assert output =~ "GHSA-rc-0002"

        Hex.State.put(:ignore_advisories, ["CVE-2026-22222"])

        File.rm!("mix.lock")
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        refute output =~ "VULNERABLE!"
        refute output =~ "GHSA-rc-0002"
        refute output =~ "Found packages with security advisories"
      end)
    end)
  end

  defmodule IgnoredRetiredDeps.MixProject do
    def project do
      [
        app: :ignored_retired_deps,
        version: "0.1.0",
        deps: [{:rc_ignored_retired, "0.1.0"}]
      ]
    end
  end

  test "deps.get does not warn about ignored retirements" do
    auth =
      Hexpm.new_user(
        "rc_retired_user",
        "rc_retired@mail.com",
        "passpass",
        "rc_retired_key"
      )

    Hexpm.new_package("hexpm", "rc_ignored_retired", "0.1.0", [], %{}, auth)

    with_project(IgnoredRetiredDeps.MixProject, fn ->
      in_tmp(fn ->
        Hex.State.put(:cache_home, tmp_path())
        Hex.State.put(:api_key, auth[:key])
        Mix.Dep.Lock.write(%{rc_ignored_retired: {:hex, :rc_ignored_retired, "0.1.0"}})

        :ok = Mix.Tasks.Deps.Get.run([])
        flush()

        :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
          :ets.insert(
            tid,
            {{:retired, "hexpm", "rc_ignored_retired", "0.1.0"},
             %{reason: :RETIRED_SECURITY, message: "Retired for testing"}}
          )

          state
        end)

        File.rm!("mix.lock")
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        assert output =~ "RETIRED!"

        Hex.State.put(:ignore_retirements, [{"rc_ignored_retired", nil}])

        File.rm!("mix.lock")
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        refute output =~ "RETIRED!"
        refute output =~ "Found retired packages"
      end)
    end)
  end

  defmodule UnchangedDeps.MixProject do
    def project do
      [
        app: :unchanged_deps,
        version: "0.1.0",
        deps: [
          {:rc_unchanged_clean, "0.1.0"},
          {:rc_unchanged_retired, "0.1.0"},
          {:rc_unchanged_advisory, "0.1.0"}
        ]
      ]
    end
  end

  test "deps.get only lists unchanged dependencies that are retired or have advisories" do
    auth =
      Hexpm.new_user(
        "rc_unchanged_user",
        "rc_unchanged@mail.com",
        "passpass",
        "rc_unchanged_key"
      )

    Hexpm.new_package("hexpm", "rc_unchanged_clean", "0.1.0", [], %{}, auth)
    Hexpm.new_package("hexpm", "rc_unchanged_retired", "0.1.0", [], %{}, auth)
    Hexpm.new_package("hexpm", "rc_unchanged_advisory", "0.1.0", [], %{}, auth)

    with_project(UnchangedDeps.MixProject, fn ->
      in_tmp(fn ->
        Hex.State.put(:cache_home, tmp_path())
        Hex.State.put(:api_key, auth[:key])

        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        assert output =~ "New:"
        assert output =~ "  rc_unchanged_clean 0.1.0"

        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        assert output =~ "Resolution completed"
        refute output =~ "Unchanged:"
        refute output =~ "  rc_unchanged_clean 0.1.0"
        refute output =~ "  rc_unchanged_retired 0.1.0"
        refute output =~ "  rc_unchanged_advisory 0.1.0"

        :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
          :ets.insert(
            tid,
            {{:retired, "hexpm", "rc_unchanged_retired", "0.1.0"},
             %{reason: :RETIRED_SECURITY, message: "Retired for testing"}}
          )

          :ets.insert(
            tid,
            {{:advisories, "hexpm", "rc_unchanged_advisory", "0.1.0"}, [@advisory]}
          )

          state
        end)

        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        assert output =~ "Unchanged:"
        refute output =~ "  rc_unchanged_clean 0.1.0"
        assert output =~ "  rc_unchanged_retired 0.1.0 RETIRED!"
        assert output =~ "  rc_unchanged_advisory 0.1.0 VULNERABLE!"
        assert output =~ "GHSA-rc-0001"
        assert output =~ "Found retired packages"
        assert output =~ "Found packages with security advisories"
      end)
    end)
  end

  defmodule EnforceLockAdvisoryDeps.MixProject do
    def project do
      [
        app: :enforce_lock_advisory_deps,
        version: "0.1.0",
        deps: [{:rc_enforce_advisory, "0.1.0"}]
      ]
    end
  end

  test "policy_enforce_lock fails deps.get and deps.update on a locked package the policy rejects" do
    auth =
      Hexpm.new_user(
        "rc_enforce_advisory_user",
        "rc_enforce_advisory@mail.com",
        "passpass",
        "rc_enforce_advisory_key"
      )

    Hexpm.new_package("hexpm", "rc_enforce_advisory", "0.1.0", [], %{}, auth)

    with_project(EnforceLockAdvisoryDeps.MixProject, fn ->
      in_tmp(fn ->
        Hex.State.put(:cache_home, tmp_path())
        Hex.State.put(:api_key, auth[:key])

        :ok = Mix.Tasks.Deps.Get.run([])
        flush()

        :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
          :ets.insert(tid, {{:advisories, "hexpm", "rc_enforce_advisory", "0.1.0"}, [@advisory]})
          state
        end)

        put_registry_policy(restriction: %{advisory_min_severity: :SEVERITY_HIGH})
        Hex.State.put(:policy_enforce_lock, true)
        lock = File.read!("mix.lock")

        Mix.Task.clear()

        assert_raise Mix.Error, ~r/Locked dependencies are rejected by the active/, fn ->
          Mix.Tasks.Deps.Get.run([])
        end

        output = shell_output()
        assert output =~ "Advisories:"
        assert output =~ "rc_enforce_advisory 0.1.0 - "
        assert output =~ "GHSA-rc-0001"
        assert File.read!("mix.lock") == lock

        Mix.Task.clear()

        assert_raise Mix.Error, ~r/Locked dependencies are rejected by the active/, fn ->
          Mix.Tasks.Deps.Update.run(["rc_enforce_advisory"])
        end

        flush()
        Hex.State.put(:ignore_advisories, ["GHSA-rc-0001"])
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])
        refute shell_output() =~ "Advisories:"

        Hex.State.put(:ignore_advisories, [])
        Hex.State.put(:policy_enforce_lock, false)
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])

        output = shell_output()
        assert output =~ "rc_enforce_advisory 0.1.0 VULNERABLE!"
        assert output =~ "Found packages with security advisories"

        Hex.State.put(:policy_enforce_lock, true)
        Hex.State.put(:policy, nil)
        Mix.Task.clear()
        :ok = Mix.Tasks.Deps.Get.run([])
      end)
    end)
  end

  defmodule EnforceLockDeniedDeps.MixProject do
    def project do
      [
        app: :enforce_lock_denied_deps,
        version: "0.1.0",
        deps: [{:rc_enforce_denied, "0.1.0"}]
      ]
    end
  end

  test "policy_enforce_lock fails on a locked package denied by the policy even when ignored" do
    auth =
      Hexpm.new_user(
        "rc_enforce_denied_user",
        "rc_enforce_denied@mail.com",
        "passpass",
        "rc_enforce_denied_key"
      )

    Hexpm.new_package("hexpm", "rc_enforce_denied", "0.1.0", [], %{}, auth)

    with_project(EnforceLockDeniedDeps.MixProject, fn ->
      in_tmp(fn ->
        Hex.State.put(:cache_home, tmp_path())
        Hex.State.put(:api_key, auth[:key])

        :ok = Mix.Tasks.Deps.Get.run([])
        flush()

        :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
          :ets.insert(
            tid,
            {{:retired, "hexpm", "rc_enforce_denied", "0.1.0"},
             %{reason: :RETIRED_DEPRECATED, message: "Retired for testing"}}
          )

          state
        end)

        put_registry_policy(
          overrides: [
            %{
              action: :OVERRIDE_ACTION_DENY,
              ref: %{package: "rc_enforce_denied"},
              comment: "Use the internal fork"
            }
          ]
        )

        Hex.State.put(:ignore_retirements, [{"rc_enforce_denied", nil}])
        Hex.State.put(:policy_enforce_lock, true)
        Mix.Task.clear()

        assert_raise Mix.Error, ~r/Locked dependencies are rejected by the active/, fn ->
          Mix.Tasks.Deps.Get.run([])
        end

        output = shell_output()
        assert output =~ "Denied:"
        assert output =~ "rc_enforce_denied 0.1.0 - "
        assert output =~ "Use the internal fork"
        refute output =~ "Retired:"

        Hex.State.put(:policy_enforce_lock, {:invalid, "yes"})
        Mix.Task.clear()

        assert_raise Mix.Error, ~r/Invalid policy_enforce_lock configuration: "yes"/, fn ->
          Mix.Tasks.Deps.Get.run([])
        end
      end)
    end)
  end

  # Serves a policy from the registry cache and marks it fetched, so
  # Hex.Policy.load/0 returns it without requesting it from the repository.
  defp put_registry_policy(repository_policy) do
    {repo, name} = {"hexpm:enforceorg", "strict-prod"}

    policy = %{
      repository: "enforceorg",
      name: name,
      visibility: :VISIBILITY_PUBLIC,
      repositories: [
        Map.merge(
          %{repository: "hexpm", restriction: %{}, overrides: []},
          Map.new(repository_policy)
        )
      ]
    }

    :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
      :ets.insert(tid, {{:policy, repo, name}, policy})
      %{state | fetched_policies: MapSet.put(state.fetched_policies, {repo, name})}
    end)

    Hex.State.put(:policy, "#{repo}/#{name}")
  end

  defmodule ChecksumIntegrity.MixProject do
    def project do
      [
        app: :checksum_integrity,
        version: "0.1.0",
        deps: [
          {:ex_doc, "~> 0.1.0"}
        ]
      ]
    end
  end

  test "raises on checksum mismatch in mix.lock" do
    in_tmp(fn ->
      Mix.Project.push(ChecksumIntegrity.MixProject)

      # First, get dependencies normally to create a valid lock file
      :ok = Mix.Tasks.Deps.Get.run([])

      # Read the lock file
      lock = Mix.Dep.Lock.read()
      {:hex, name, version, inner_checksum, managers, deps, repo, outer_checksum} = lock[:ex_doc]

      assert_checksum_mismatch(%{
        ex_doc:
          {:hex, name, version, invalid_checksum(inner_checksum), managers, deps, repo,
           outer_checksum}
      })

      assert_checksum_mismatch(%{
        ex_doc:
          {:hex, name, version, inner_checksum, managers, deps, repo,
           invalid_checksum(outer_checksum)}
      })
    end)
  end

  defp assert_checksum_mismatch(lock) do
    File.write!("mix.lock", inspect(lock, limit: :infinity, pretty: true))
    Mix.Task.clear()

    # The bug causes this to silently pass and rewrite the lock file with correct checksums
    assert_raise Mix.Error, ~r/Registry checksum mismatch against lock/, fn ->
      Mix.Tasks.Deps.Get.run([])
    end
  end

  defp invalid_checksum("0" <> rest), do: "1" <> rest
  defp invalid_checksum(<<_::binary-size(1), rest::binary>>), do: "0" <> rest
end
