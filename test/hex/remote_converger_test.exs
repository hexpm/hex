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
    Hex.OnceCache.clear(Hex.OAuth.RefreshCache)

    Hex.OAuth.store_token(%{
      "access_token" => "expired_access_token",
      "refresh_token" => "invalid_refresh_token",
      "expires_at" => System.system_time(:second) - 3600
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
    end)
  end

  test "auth preflight is skipped for public repos" do
    assert Hex.RemoteConverger.auth_preflight_required?([{"hexpm", "postgrex"}]) == false
  end

  test "auth preflight is required for organization repos without repo auth" do
    in_tmp(fn ->
      set_home_cwd()

      assert Hex.RemoteConverger.auth_preflight_required?([
               {"hexpm:remote_converger_org", "private_prompt_pkg"}
             ])
    end)
  end

  test "auth preflight is skipped when repo auth is already available" do
    in_tmp(fn ->
      set_home_cwd()

      auth = new_repo_auth_user("remote_converger_repo_auth_preflight")

      repos = Hex.State.fetch!(:repos)
      repos = put_in(repos["hexpm"].auth_key, auth[:key])
      Hex.State.put(:repos, repos)

      assert Hex.RemoteConverger.auth_preflight_required?([
               {"hexpm:remote_converger_org", "private_prompt_pkg"}
             ]) == false
    end)
  end

  test "auth preflight is skipped for untrusted org repos" do
    in_tmp(fn ->
      set_home_cwd()
      Hex.State.put(:mirror_url, "http://example.com")

      assert Hex.RemoteConverger.auth_preflight_required?([
               {"hexpm:remote_converger_org", "private_prompt_pkg"}
             ]) == false
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
