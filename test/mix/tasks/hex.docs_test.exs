defmodule Mix.Tasks.Hex.DocsTest do
  use HexTest.Case
  @moduletag :integration

  defmodule ExampleDeps.MixProject do
    def project do
      [
        app: :example_app,
        version: "0.1.0",
        deps: []
      ]
    end
  end

  setup_all do
    auth = Hexpm.new_key(user: "user", pass: "hunter42")
    Hexpm.new_package("docs_package", "1.1.1", %{}, %{}, auth)
    Hexpm.new_package("docs_package", "1.1.2", %{}, %{}, auth)

    :ok
  end

  test "fetch and open all packages in this applications deps" do
    Mix.Project.push(ExampleDeps.MixProject)
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.2"}})
      Mix.Tasks.Hex.Docs.run(["fetch"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")
    end)
  end

  test "fetch the version of a dependency from this apps lock file" do
    Mix.Project.push(ExampleDeps.MixProject)
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.2"}})
      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")
    end)
  end

  test "fetch the latest version of a package" do
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")

      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package"])
      already_fetched_msg = "Docs already fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "fetch the latest version of a package using the latest flag" do
    Mix.Project.push(ExampleDeps.MixProject)
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.1"}})
      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package", "--latest"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")
    end)
  end

  test "fetch a specific version of a package" do
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package", "1.1.2"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")

      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package", "1.1.2"])
      already_fetched_msg = "Docs already fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "fetch a package that does not exist" do
    assert_raise Mix.Error, "No package with name package_not_found", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", "package_not_found"])
    end
  end

  test "invalid arguments for docs task" do
    assert_raise Mix.Error, ~r"Invalid arguments", fn ->
      Mix.Tasks.Hex.Docs.run(["invalid", "command"])
    end
  end

  test "fetch and open tasks fails when package name is not provided" do
    msg = "Specify a package name or run inside a Mix project to fetch docs for all dependencies"

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["fetch"])
    end

    assert_raise Mix.Error, "You must specify the name of a package", fn ->
      Mix.Tasks.Hex.Docs.run(["offline"])
    end
  end

  test "offline task fails when docs not found" do
    message = "No package with name decimal or version 1.1.2"

    assert_raise Mix.Error, message, fn ->
      Mix.Tasks.Hex.Docs.run(["offline", "decimal", "1.1.2"])
    end
  end

  test "open latest version offline using offline task" do
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", "docs_package"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      browser_open_msg = "#{docs_home}/docs_package/1.1.2/index.html"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, [^browser_open_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")
    end)
  end

  test "offline package with version succeeds when package is available remotely" do
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", "docs_package", "1.1.2"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      browser_open_msg = "#{docs_home}/docs_package/1.1.2/index.html"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, [^browser_open_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")

      Mix.Tasks.Hex.Docs.run(["fetch", "docs_package", "1.1.2"])
      already_fetched_msg = "Docs already fetched: #{docs_home}/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "open raises an error" do
    msg = """
    Open has been removed, use one of:

    mix hex.docs online PACKAGE [VERSION]
    mix hex.docs offline PACKAGE [VERSION]
    """

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["open", "decimal"])
    end
  end

  test "open docs online" do
    Mix.Tasks.Hex.Docs.run(["online", "ecto"])
    assert_received {:hex_system_cmd, _cmd, ["https://hexdocs.pm/ecto"]}
  end

  test "open the version of a package this app uses online" do
    Mix.Project.push(ExampleDeps.MixProject)
    Hex.State.put(:home, tmp_path())

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.1"}})
      Mix.Tasks.Hex.Docs.run(["online", "docs_package"])
      assert_received {:hex_system_cmd, _cmd, ["https://hexdocs.pm/docs_package/1.1.1"]}
    end)
  end

  test "open latest version of a package this app uses online" do
    Mix.Project.push(ExampleDeps.MixProject)
    Hex.State.put(:home, tmp_path())

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.1"}})
      Mix.Tasks.Hex.Docs.run(["online", "docs_package", "--latest"])
      assert_received {:hex_system_cmd, _cmd, ["https://hexdocs.pm/docs_package"]}
    end)
  end

  test "open the version of a package this app uses offline" do
    Mix.Project.push(ExampleDeps.MixProject)
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.2"}})
      Mix.Tasks.Hex.Docs.run(["offline", "docs_package"])
      fetched_msg = "Docs fetched: #{docs_home}/docs_package/1.1.2"
      browser_open_msg = "#{docs_home}/docs_package/1.1.2/index.html"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, [^browser_open_msg]}
      assert File.exists?("#{docs_home}/docs_package/1.1.2")
    end)
  end
end
