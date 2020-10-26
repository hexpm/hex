defmodule Mix.Tasks.Hex.SponsorTest do
  use HexTest.Case
  @moduletag :integration

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

    assert String.match?(header_output, ~r/Dependency/)
    assert String.match?(header_output, ~r/Sponsorship/)

    assert String.match?(package_line, ~r/#{@package_name}/)
    assert String.match?(package_line, ~r/#{url}/)
  end

  test "without sponsor link", context do
    with_test_package("1.2.0", %{links: %{github: "https://github.com/foo/bar"}}, context, fn ->
      Mix.Task.run("hex.sponsor")
    end)

    assert_received {:mix_shell, :info, [header_output]}
    refute_received {:mix_shell, :info, [_package_line]}

    assert String.match?(header_output, ~r/No dependencies with sponsorship links found/)
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
      Mix.Tasks.Hex.update_keys(auth[:"$write_key"], auth[:"$read_key"])
      Mix.Dep.Lock.write(%{@package => {:hex, @package, version}})

      Mix.Task.run("deps.get")
      flush()

      fun.()
    end)
  end
end
