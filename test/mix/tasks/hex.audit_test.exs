defmodule Mix.Tasks.Hex.AuditTest do
  use HexTest.IntegrationCase

  @package :test_package
  @package_name Atom.to_string(@package)

  @advisory %{
    id: "GHSA-test-0001",
    summary: "Remote code execution via crafted input",
    html_url: "https://github.com/advisories/GHSA-test-0001",
    severity: :SEVERITY_HIGH,
    api_url: "https://hex.pm/api/advisories/GHSA-test-0001"
  }

  defmodule RetiredDeps.MixProject do
    def project do
      [app: :test_app, version: "0.0.1", deps: [{:test_package, ">= 0.1.0"}]]
    end
  end

  setup_all do
    auth = Hexpm.new_user("audit_user", "audit@mail.com", "passpass", "key")
    {:ok, [auth: auth]}
  end

  test "audit (retired package without a message)", context do
    with_test_package("0.1.0", context, fn ->
      retire_test_package("0.1.0", "security")

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}
      assert_retired_section(@package_name, "0.1.0", "(security)")
      assert_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (retired package with a custom message)", context do
    with_test_package("0.2.0", context, fn ->
      retire_test_package("0.2.0", "invalid", "Superseded by v1.0.0")

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}
      assert_retired_section(@package_name, "0.2.0", "(invalid) Superseded by v1.0.0")
      assert_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (no retired packages)", context do
    with_test_package("1.0.0", context, fn ->
      Mix.Task.run("hex.audit")
      assert_received {:mix_shell, :info, ["No retired or security advisory packages found"]}
    end)
  end

  test "audit (package with a security advisory)", context do
    with_test_package("1.1.0", context, fn ->
      inject_advisory(@package_name, "1.1.0", [@advisory])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      assert_advisory_section(@package_name, "1.1.0", @advisory)
      assert_received {:mix_shell, :error, ["Found packages with security advisories"]}
      refute_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (package with both retirement and security advisory)", context do
    with_test_package("1.2.0", context, fn ->
      :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
        :ets.insert(
          tid,
          {{:retired, "hexpm", @package_name, "1.2.0"}, %{reason: :RETIRED_SECURITY}}
        )

        :ets.insert(tid, {{:advisories, "hexpm", @package_name, "1.2.0"}, [@advisory]})
        state
      end)

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      assert_retired_section(@package_name, "1.2.0", "(security)")
      assert_advisory_section(@package_name, "1.2.0", @advisory)
      assert_received {:mix_shell, :error, ["Found retired packages"]}
      assert_received {:mix_shell, :error, ["Found packages with security advisories"]}
    end)
  end

  test "audit (package with multiple advisories)", context do
    other_advisory = %{
      id: "GHSA-test-0002",
      summary: "Another vulnerability",
      html_url: "https://github.com/advisories/GHSA-test-0002",
      severity: :SEVERITY_MEDIUM
    }

    with_test_package("1.3.0", context, fn ->
      inject_advisory(@package_name, "1.3.0", [@advisory, other_advisory])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      assert_advisory_section(@package_name, "1.3.0", @advisory)
      assert_advisory_section(@package_name, "1.3.0", other_advisory)
    end)
  end

  test "audit (advisories sharing a CVE alias are deduplicated)", context do
    eef_advisory = %{
      id: "EEF-CVE-2026-32689",
      summary: "EEF source",
      html_url: "https://osv.dev/vulnerability/EEF-CVE-2026-32689",
      severity: :SEVERITY_HIGH,
      api_url: "https://api.osv.dev/v1/vulns/EEF-CVE-2026-32689",
      aliases: ["CVE-2026-32689", "GHSA-628h-q48j-jr6q"],
      references: []
    }

    ghsa_advisory = %{
      id: "GHSA-628h-q48j-jr6q",
      summary: "GHSA source",
      html_url: "https://osv.dev/vulnerability/GHSA-628h-q48j-jr6q",
      severity: :SEVERITY_HIGH,
      api_url: "https://api.osv.dev/v1/vulns/GHSA-628h-q48j-jr6q",
      aliases: ["CVE-2026-32689"],
      references: []
    }

    with_test_package("1.4.0", context, fn ->
      inject_advisory(@package_name, "1.4.0", [eef_advisory, ghsa_advisory])

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}

      grouped_advisory = %{
        id: "EEF-CVE-2026-32689",
        summary: "EEF source",
        html_url: "https://osv.dev/vulnerability/EEF-CVE-2026-32689",
        severity: :SEVERITY_HIGH,
        api_url: "https://api.osv.dev/v1/vulns/EEF-CVE-2026-32689",
        aliases: [
          %{id: "CVE-2026-32689", url: :undefined},
          %{
            id: "GHSA-628h-q48j-jr6q",
            url: "https://osv.dev/vulnerability/GHSA-628h-q48j-jr6q"
          }
        ]
      }

      assert_advisory_section(@package_name, "1.4.0", grouped_advisory)

      refute_received {:mix_shell, :info,
                       ["  " <> @package_name <> " 1.4.0 - GHSA-628h-q48j-jr6q" <> _]}
    end)
  end

  def with_test_package(version, %{auth: auth}, fun) do
    Mix.Project.push(RetiredDeps.MixProject)

    Hexpm.new_package("hexpm", @package_name, version, [], %{}, auth)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      Hex.State.put(:api_key, auth[:key])
      Mix.Dep.Lock.write(%{@package => {:hex, @package, version}})

      Mix.Task.run("deps.get")
      flush()
      fun.()
    end)
  end

  defp retire_test_package(version, reason, message \\ "") do
    Mix.Tasks.Hex.Retire.run([@package_name, version, reason, "--message", message])

    # Mix does not support the RemoteConverger.post_converge/0 callback on Elixir < 1.4,
    # so we need to explicitly reset the registry.
    Hex.Registry.Server.close()
  end

  defp inject_advisory(package, version, advisories) do
    :sys.replace_state(Hex.Registry.Server, fn %{ets: tid} = state ->
      :ets.insert(tid, {{:advisories, "hexpm", package, version}, advisories})
      state
    end)
  end

  defp assert_retired_section(package, version, message) do
    header = render([:bright, "Retired:", :reset])
    row = render(["  #{package} #{version} - ", :yellow, message, :reset])

    assert_received {:mix_shell, :info, [^header]}
    assert_received {:mix_shell, :info, [^row]}
  end

  defp assert_advisory_section(package, version, advisory) do
    block =
      render([
        "  #{package} #{version} - " | Hex.Utils.format_advisory_ansi(advisory, "    ")
      ])

    assert_received {:mix_shell, :info, [^block]}
  end

  defp render(message) do
    message |> Hex.Shell.format() |> IO.iodata_to_binary()
  end
end
