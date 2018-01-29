defmodule Mix.Tasks.Hex.AuditTest do
  use HexTest.Case
  @moduletag :integration

  @package :test_package
  @package_name Atom.to_string(@package)

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
      assert_received {:mix_shell, :info, ["No retired packages found"]}
    end)
  end

  def with_test_package(version, %{auth: auth}, fun) do
    Mix.Project.push(RetiredDeps.MixProject)

    Hexpm.new_package(@package_name, version, [], %{}, auth)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      Mix.Tasks.Hex.update_key(auth[:"$encrypted_key"])
      Mix.Dep.Lock.write(%{@package => {:hex, @package, version}})

      Mix.Task.run("deps.get")
      flush()
      fun.()
    end)
  end

  defp retire_test_package(version, reason, message \\ "") do
    send(self(), {:mix_shell_input, :prompt, "passpass"})
    Mix.Tasks.Hex.Retire.run([@package_name, version, reason, "--message", message])

    # Mix does not support the RemoteConverger.post_converge/0 callback on Elixir < 1.4,
    # so we need to explicitly reset the registry.
    Hex.Registry.Server.close()
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
end
