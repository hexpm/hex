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
      assert_output_row(@package_name, "0.1.0", "(security)")
      assert_received {:mix_shell, :error, ["Found retired packages"]}
    end)
  end

  test "audit (retired package with a custom message)", context do
    with_test_package("0.2.0", context, fn ->
      retire_test_package("0.2.0", "invalid", "Superseded by v1.0.0")

      assert catch_throw(Mix.Task.run("hex.audit")) == {:exit_code, 1}
      assert_output_row(@package_name, "0.2.0", "(invalid) Superseded by v1.0.0")
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

      expected =
        "GHSA-test-0001 (HIGH): Remote code execution via crafted input - https://github.com/advisories/GHSA-test-0001"

      assert_advisory_output_row(@package_name, "1.1.0", expected)
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
      assert_received {:mix_shell, :error, ["Found retired packages"]}
      assert_received {:mix_shell, :error, ["Found packages with security advisories"]}
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

  defp assert_output_row(package, version, message) do
    whitespace_length = String.length("Retirement reason  ") - String.length(message)
    whitespace_length = if whitespace_length < 2, do: 2, else: whitespace_length

    output =
      [
        [package, :reset, "  "],
        [version, :reset, "    "],
        [message, :reset, String.duplicate(" ", whitespace_length)]
      ]
      |> IO.ANSI.format()
      |> List.to_string()

    assert_received {:mix_shell, :info, [^output]}
  end

  defp assert_advisory_output_row(package, version, advisory_text) do
    col1_width = max(String.length("Dependency"), String.length(package))
    col2_width = max(String.length("Version"), String.length(version))
    col3_width = max(String.length("Advisory"), String.length(advisory_text))

    output =
      [
        [package, :reset, String.duplicate(" ", col1_width - String.length(package) + 2)],
        [version, :reset, String.duplicate(" ", col2_width - String.length(version) + 2)],
        [
          advisory_text,
          :reset,
          String.duplicate(" ", col3_width - String.length(advisory_text) + 2)
        ]
      ]
      |> IO.ANSI.format()
      |> List.to_string()

    assert_received {:mix_shell, :info, [^output]}
  end
end
