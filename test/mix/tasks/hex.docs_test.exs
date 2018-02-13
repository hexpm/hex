defmodule Mix.Tasks.Hex.DocsTest do
  use HexTest.Case
  @moduletag :integration

  test "fetch and open the latest version of a package" do
    package = "docs_package"
    old_version = "1.1.1"
    latest_version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    auth = Hexpm.new_key(user: "user", pass: "hunter42")
    Hexpm.new_package(package, old_version, %{}, %{}, auth)
    Hexpm.new_package(package, latest_version, %{}, %{}, auth)

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package])
      fetched_msg = "Docs fetched: #{docs_home}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "fetch and open a specific version of a package" do
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())

    docs_home =
      :home
      |> Hex.State.fetch!()
      |> Path.join("docs")

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      fetched_msg = "Docs fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "fetch a package that does not exists" do
    package = "package_not_found"

    not_found_msg = "No package with name #{package}"

    assert_raise Mix.Error, not_found_msg, fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package])
    end
  end

  test "invalid arguments for docs task" do
    assert_raise Mix.Error, ~r"Invalid arguments", fn ->
      Mix.Tasks.Hex.Docs.run(["invalid", "command"])
    end
  end

  test "fetch and open tasks fails when package name is not provided" do
    msg = "You must specify at least the name of a package"

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["fetch"])
    end

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["offline"])
    end
  end

  test "offline task fails when docs not found" do
    package = "decimal"
    version = "1.1.2"
    message = "No package with name decimal or version 1.1.2"

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Hex.Docs.run(["offline", package, version])
    end
  end

  test "open latest version offline using offline task" do
    package = "docs_package"
    old_version = "1.1.1"
    latest_version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    auth = Hexpm.new_key(user: "user", pass: "hunter42")
    Hexpm.new_package(package, old_version, %{}, %{}, auth)
    Hexpm.new_package(package, latest_version, %{}, %{}, auth)

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", package])
      fetched_msg = "Docs fetched: #{docs_home}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}
    end)
  end

  test "offline package with version succeeds when package is available remotely" do
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())

    docs_home =
      :home
      |> Hex.State.fetch!()
      |> Path.join("docs")

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", package, version])
      fetched_msg = "Docs fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "open raises an error" do
    msg = """
    Open has been removed, use one of:\n\nmix hex.docs online PACKAGE [VERSION]
    mix hex.docs offline PACKAGE [VERSION]
    """

    package = "decimal"

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["open", package])
    end
  end
end
