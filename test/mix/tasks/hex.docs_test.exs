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
    Hexpm.new_package("hexpm", "docs_package", "1.1.1", %{}, %{}, auth)
    Hexpm.new_package("hexpm", "docs_package", "1.1.2", %{}, %{}, auth)
    Hexpm.new_package("hexpm", "docs_package", "2.0.0-rc1", %{}, %{}, auth)
    Hexpm.new_package("hexpm", "pre_only_package", "0.0.1-rc1", %{}, %{}, auth)
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
      fetched_msg = "Docs fetched: #{docs_home}/hexpm/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/hexpm/docs_package/1.1.2")
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
      fetched_msg = "Docs fetched: #{docs_home}/hexpm/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/hexpm/docs_package/1.1.2")
    end)
  end

  test "fetch the latest version of a package" do
    package = "docs_package"
    latest_version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package])
      fetched_msg = "Docs fetched: #{docs_home}/#{org_dir}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package])

      already_fetched_msg =
        "Docs already fetched: #{docs_home}/#{org_dir}/#{package}/#{latest_version}"

      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "when the only release is a pre-release, return that version" do
    package = "pre_only_package"
    latest_version = "0.0.1-rc1"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package])
      fetched_msg = "Docs fetched: #{docs_home}/#{org_dir}/#{package}/#{latest_version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package])

      already_fetched_msg =
        "Docs already fetched: #{docs_home}/#{org_dir}/#{package}/#{latest_version}"

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
      fetched_msg = "Docs fetched: #{docs_home}/hexpm/docs_package/1.1.2"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert File.exists?("#{docs_home}/hexpm/docs_package/1.1.2")
    end)
  end

  test "fetch a specific version of a package" do
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      fetched_msg = "Docs fetched: #{docs_home}/#{org_dir}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^fetched_msg]}

      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{org_dir}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "fetch a specific version of a package that exists in the fallback location" do
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())

    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      File.cp_r!("#{docs_home}/#{org_dir}/#{package}", "#{docs_home}/#{package}")
      File.rm_rf!("#{docs_home}/#{org_dir}/#{package}/#{version}")

      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "fetch a package that does not exist" do
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
    msg = "Specify a package name or run inside a Mix project to fetch docs for all dependencies"

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Docs.run(["fetch"])
    end

    assert_raise Mix.Error, "You must specify the name of a package", fn ->
      Mix.Tasks.Hex.Docs.run(["offline"])
    end
  end

  test "offline task fails when docs not found" do
    Mix.Tasks.Hex.Docs.run(["offline", "decimal", "1.1.2"])

    message = "Couldn't find docs for package with name decimal or version 1.1.2"
    assert_received {:mix_shell, :error, [^message]}
  end

  test "offline task fails when index file in docs not found" do
    Mix.Tasks.Hex.Docs.run(["offline", "decimal", "1.1.2"])

    message = "Couldn't find docs for package with name decimal or version 1.1.2"
    assert_received {:mix_shell, :error, [^message]}
  end

  test "open latest version offline using offline task" do
    package = "docs_package"
    latest_version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", package])
      fetched_msg = "Docs fetched: #{docs_home}/#{org_dir}/#{package}/#{latest_version}"
      browser_open_msg = "#{docs_home}/#{org_dir}/#{package}/#{latest_version}/index.html"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == browser_open_msg
    end)
  end

  test "open latest version in epub offline using offline task" do
    package = "docs_package"
    latest_version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", package, "--format", "epub"])
      fetched_msg = "Docs fetched: #{docs_home}/#{org_dir}/#{package}/#{latest_version}"
      browser_open_msg = "#{docs_home}/#{org_dir}/#{package}/#{latest_version}/docs_package.epub"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == browser_open_msg
    end)
  end

  test "offline package with version succeeds when package is available remotely" do
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())

    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["offline", package, version])
      fetched_msg = "Docs fetched: #{docs_home}/#{org_dir}/#{package}/#{version}"
      browser_open_msg = "#{docs_home}/#{org_dir}/#{package}/#{version}/index.html"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == browser_open_msg

      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      already_fetched_msg = "Docs already fetched: #{docs_home}/#{org_dir}/#{package}/#{version}"
      assert_received {:mix_shell, :info, [^already_fetched_msg]}
    end)
  end

  test "offline package with version uses fallback location" do
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())

    docs_home = Path.join(Hex.State.fetch!(:home), "docs")
    org_dir = "hexpm"

    in_tmp("docs", fn ->
      Mix.Tasks.Hex.Docs.run(["fetch", package, version])
      File.cp_r!("#{docs_home}/#{org_dir}/#{package}", "#{docs_home}/#{package}")
      File.rm_rf!("#{docs_home}/#{org_dir}/#{package}/#{version}")

      Mix.Tasks.Hex.Docs.run(["offline", package, version])
      browser_open_msg = "#{docs_home}/#{package}/#{version}/index.html"
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == browser_open_msg
    end)
  end

  test "open docs online" do
    Mix.Tasks.Hex.Docs.run(["online", "ecto"])
    assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
    assert Enum.fetch!(browser_open_cmd, -1) == "https://hexdocs.pm/ecto"
  end

  test "open the version of a package this app uses online" do
    Mix.Project.push(ExampleDeps.MixProject)
    Hex.State.put(:home, tmp_path())

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.1"}})
      Mix.Tasks.Hex.Docs.run(["online", "docs_package"])
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == "https://hexdocs.pm/docs_package/1.1.1"
    end)
  end

  test "open latest version of a package this app uses online" do
    Mix.Project.push(ExampleDeps.MixProject)
    Hex.State.put(:home, tmp_path())

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, "1.1.1"}})
      Mix.Tasks.Hex.Docs.run(["online", "docs_package", "--latest"])
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == "https://hexdocs.pm/docs_package"
    end)
  end

  test "open the version of a package this app uses offline" do
    Mix.Project.push(ExampleDeps.MixProject)
    package = "docs_package"
    version = "1.1.2"
    bypass_mirror()
    Hex.State.put(:home, tmp_path())
    docs_home = Path.join(Hex.State.fetch!(:home), "docs")

    in_tmp("docs", fn ->
      Mix.Dep.Lock.write(%{docs_package: {:hex, :docs_package, version}})
      Mix.Tasks.Hex.Docs.run(["offline", package])
      fetched_msg = "Docs fetched: #{docs_home}/hexpm/#{package}/#{version}"
      browser_open_msg = "#{docs_home}/hexpm/#{package}/#{version}/index.html"
      assert_received {:mix_shell, :info, [^fetched_msg]}
      assert_received {:hex_system_cmd, _cmd, browser_open_cmd}
      assert Enum.fetch!(browser_open_cmd, -1) == browser_open_msg
      assert File.exists?("#{docs_home}/hexpm/#{package}/#{version}")
    end)
  end
end
