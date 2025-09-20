defmodule Mix.Tasks.Hex.SponsorTest do
  use HexTest.IntegrationCase

  @package :test_sponsored_package
  @package_name Atom.to_string(@package)

  defmodule SponsoredDeps.MixProject do
    def project do
      [app: :test_app, version: "0.0.1", deps: [{:test_sponsored_package, ">= 0.1.0"}]]
    end
  end

  setup_all do
    auth = Hexpm.new_user("sponsor_user", "sponsor@mail.com", "passpass", "key")
    {:ok, [auth: auth]}
  end

  test "sponsor with sponsor link", context do
    url = "https://sponsor.foo.bar"

    with_test_package("0.1.0", %{links: %{sponsor: url}}, context, fn ->
      Mix.Task.run("hex.sponsor")
    end)

    assert_received {:mix_shell, :info, [header_output]}
    assert_received {:mix_shell, :info, [package_line]}

    assert header_output =~ "Dependency"
    assert header_output =~ "Sponsorship"

    assert package_line =~ @package_name
    assert package_line =~ url
  end

  test "without sponsor link", context do
    with_test_package("1.2.0", %{links: %{github: "https://github.com/foo/bar"}}, context, fn ->
      Mix.Task.run("hex.sponsor")
    end)

    assert_received {:mix_shell, :info, [header_output]}
    refute_received {:mix_shell, :info, [_package_line]}

    assert header_output =~ "No dependencies with sponsorship link found"
  end

  test "outside a mix project", %{auth: auth} do
    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      Mix.Tasks.Hex.update_keys(auth[:key], auth[:key])
      flush()

      error_msg =
        "The sponsor task only works inside a Mix project. " <>
          "Please ensure you are in a directory with a mix.exs file."

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Sponsor.run([])
      end
    end)
  end

  defp with_test_package(version, metadata, %{auth: auth}, fun) do
    Mix.Project.push(SponsoredDeps.MixProject)

    Hexpm.new_package(
      "hexpm",
      @package_name,
      version,
      [],
      metadata,
      auth
    )

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      Mix.Tasks.Hex.update_keys(auth[:key], auth[:key])
      Mix.Dep.Lock.write(%{@package => {:hex, @package, version}})

      Mix.Task.run("deps.get")
      flush()

      fun.()
    end)
  end
end
