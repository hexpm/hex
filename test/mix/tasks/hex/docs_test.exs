defmodule Mix.Tasks.Hex.DocsTest do
  use HexTest.Case
  @moduletag :integration

  test "open fails when docs not found" do
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    package = "decimal"
    version = "1.1.2"
    message = "Documentation file not found: #{docs_home}/#{package}/#{version}/index.html"
    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Hex.Docs.run(["open", package, version, "--offline"])
    end
  end

  test "fetch and open the latest version of a package" do
    package = "package"
    old_version = "1.1.1"
    latest_version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    auth = HexWeb.new_key([user: "user", pass: "hunter42"])
    HexWeb.new_package(package, old_version, %{}, %{}, auth)
    HexWeb.new_package(package, latest_version, %{}, %{}, auth)

    in_tmp "docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package])
      fetched_msg = "Docs fetched: #{docs_home}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end
  end

  test "fetch and open a specific version of a package" do
    package = "package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())

    docs_home = :home |> Hex.State.fetch!() |> Path.join("docs")

    in_tmp "docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      fetched_msg = "Docs fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end
  end

  test "fetch a package that does not exists" do
    package = "package_not_found"

    not_found_msg = "No package with name #{package}"
    assert_raise Mix.Error, not_found_msg, fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package])
    end
  end

  test "invalid arguments for docs task" do
    exception = assert_raise Mix.Error, fn -> Mix.Tasks.Hex.Docs.run([]) end
    assert Exception.message(exception) =~ ~s([deprecation] The "mix hex.docs" command has changed)

    invalid_args_msg = """
    Invalid arguments, expected one of:
    mix hex.docs fetch PACKAGE [VERSION]
    mix hex.docs open PACKAGE [VERSION]
    """

    assert_raise Mix.Error, invalid_args_msg, fn ->
      Mix.Tasks.Hex.Docs.run(["invalid", "command"])
    end
  end

  test "fetch and open tasks fails when package name is not provided" do
    msg = "You must specify at least the name of a package"
    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["fetch"])
    end

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["open", "--offline"])
    end
  end
end
