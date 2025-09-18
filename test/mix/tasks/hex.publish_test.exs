defmodule Mix.Tasks.Docs do
  def run(_) do
    File.mkdir_p!("doc")
    File.write!("doc/index.html", "the index")
  end
end

defmodule Mix.Tasks.Hex.PublishTest do
  use HexTest.IntegrationCase

  defmodule DocsSimple.MixProject do
    def project do
      [app: :ex_doc, version: "0.1.0", aliases: [docs: [&docs/1]]]
    end

    defp docs(_) do
      File.mkdir_p!("doc")
      File.write!("doc/index.html", "the index")
    end
  end

  defmodule DocsError.MixProject do
    def project do
      [app: :ex_doc, version: "0.1.1", aliases: [docs: [&docs/1]]]
    end

    defp docs(_) do
      File.mkdir_p!("doc")
      File.write!("doc/index.html", "the index")
    end
  end

  defmodule DocsFilenameError.MixProject do
    def project do
      [app: :invalid_filename, version: "0.1.0", aliases: [docs: [&docs/1]]]
    end

    defp docs(_) do
      File.mkdir_p!("doc")
      File.write!("doc/index.html", "the index")
      File.write!("doc/1.5.5", "")
    end
  end

  defmodule DocsDirnameError.MixProject do
    def project do
      [app: :invalid_dirname, version: "0.1.0", aliases: [docs: [&docs/1]]]
    end

    defp docs(_) do
      File.mkdir_p!("doc/1.5.5")
      File.write!("doc/index.html", "the index")
      File.write!("doc/1.5.5/index.html", "")
    end
  end

  defmodule DocsOutputConfigured.MixProject do
    def project do
      [
        app: :ex_doc,
        version: "0.1.0",
        aliases: [docs: [&docs/1]],
        docs: [output: "my_docs"]
      ]
    end

    defp docs(_) do
      File.mkdir_p!("my_docs")
      File.write!("my_docs/index.html", "the index")
    end
  end

  defmodule DocsOutputConfiguredFunction.MixProject do
    def project do
      [
        app: :ex_doc,
        version: "0.1.0",
        aliases: [docs: [&docs/1]],
        docs: &docs_config/0
      ]
    end

    defp docs(_) do
      File.mkdir_p!("my_docs")
      File.write!("my_docs/index.html", "the index")
    end

    defp docs_config, do: [output: "my_docs"]
  end

  defmodule DocsOutputNoOutput.MixProject do
    def project do
      [
        app: :ex_doc,
        version: "0.1.0",
        aliases: [docs: [&docs/1]],
        docs: [output: "my_docs"]
      ]
    end

    defp docs(_), do: :ok
  end

  test "ensure user exists" do
    Process.put(:hex_test_app_name, :publish_ensure_user_exists)
    Mix.Project.push(ReleaseSimple.MixProject)
    set_home_path(tmp_path("does_not_exist"))

    in_tmp(fn ->
      File.write!("myfile.txt", "hello")
      send(self(), {:mix_shell_input, :yes?, false})

      assert_raise Mix.Error, "No authenticated user found. Run `mix hex.user auth`", fn ->
        Mix.Tasks.Hex.Publish.run([])
      end
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create and revert a package" do
    Process.put(:hex_test_app_name, :publish_and_revert)
    Mix.Project.push(ReleaseNewSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")
      File.write_stat!("mix.exs", %{File.stat!("mix.exs") | mode: 0o100644})
      File.write_stat!("myfile.txt", %{File.stat!("myfile.txt") | mode: 0o100644})
      setup_auth("user2", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info,
                       [
                         "Package published to http://localhost:4043/packages/publish_and_revert/0.0.1 " <>
                           _
                       ]}

      assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "publish_and_revert", "0.0.1")

      assert_received {:mix_shell, :info,
                       [
                         "Before publishing, please read the Code of Conduct: https://hex.pm/policies/codeofconduct\n"
                       ]}

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--revert", "0.0.1"])
      assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "publish_and_revert", "0.0.1")
    end)
  after
    purge([ReleaseNewSimple.MixProject])
  end

  test "create a package without confirming" do
    Process.put(:hex_test_app_name, :publish_without_confirmation)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")
      setup_auth("user2", "hunter42")
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace", "--yes"])

      assert {:ok, {200, _, _}} =
               Hex.API.Release.get("hexpm", "publish_without_confirmation", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create and revert docs" do
    Mix.Project.push(DocsSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info,
                       ["Docs published to http://localhost:4043/docs/ex_doc-0.1.0.tar.gz"]}

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--revert", "0.1.0"])
      assert_received {:mix_shell, :info, ["Reverted docs for ex_doc 0.1.0"]}
    end)
  end

  test "try create existing package without permissions" do
    Process.put(:hex_test_app_name, :ex_doc)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")
      setup_auth("user2", "hunter42")
      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      assert catch_throw(
               Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace", "--yes"])
             ) == {:exit_code, 1}

      message =
        "Package with name ex_doc already exists. " <>
          "Make sure you are authenticated and have permissions to publish the package."

      assert_received {:mix_shell, :error, ["Publishing failed"]}
      assert_received {:mix_shell, :error, [^message]}
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "publish docs with invalid filename" do
    Mix.Project.push(DocsFilenameError.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      error_msg = "Invalid filename: top-level filenames cannot match a semantic version pattern"

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

        refute_received {:mix_shell, :info,
                         ["Docs published to https://hexdocs.pm/invalid_filename/0.1.0"]}
      end
    end)
  end

  test "publish docs with invalid dirname" do
    Mix.Project.push(DocsDirnameError.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      error_msg = "Invalid filename: top-level filenames cannot match a semantic version pattern"

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

        refute_received {:mix_shell, :info,
                         ["Docs published to https://hexdocs.pm/invalid_dirname/0.1.0"]}
      end
    end)
  end

  test "raises if output folder is missing" do
    Mix.Project.push(DocsOutputNoOutput.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      error_msg = "File not found: my_docs/index.html"

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

        refute_received {:mix_shell, :info, ["Docs published to https://hexdocs.pm/ex_doc/0.1.0"]}
      end
    end)
  end

  test "publishes docs with different output configured" do
    Mix.Project.push(DocsOutputConfigured.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info,
                       ["Docs published to http://localhost:4043/docs/ex_doc-0.1.0.tar.gz"]}
    end)
  end

  test "publishes docs when docs config is a function" do
    Mix.Project.push(DocsOutputConfiguredFunction.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info,
                       ["Docs published to http://localhost:4043/docs/ex_doc-0.1.0.tar.gz"]}
    end)
  end

  test "docs when package is not published yet" do
    Mix.Project.push(DocsError.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress", "--replace"])

      message = "Publishing docs failed due to the package not being published yet"
      assert_received {:mix_shell, :error, [^message]}

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--revert", "0.1.1"])
      assert_received {:mix_shell, :info, ["Docs do not exist"]}
    end)
  end

  test "package create with package name" do
    Process.put(:hex_test_package_name, :publish_package_name)
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")
      setup_auth("user2", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      assert_raise Mix.Error, ~r"Invalid arguments", fn ->
        Mix.Tasks.Hex.Publish.run(["invalid", "--no-progress", "--replace"])
      end

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["--no-progress", "--replace"])
      assert_received {:mix_shell, :info, ["Building publish_package_name 0.0.1"]}

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])

      assert {:ok, {200, _, body}} = Hex.API.Release.get("hexpm", "publish_package_name", "0.0.1")
      assert body["meta"]["app"] == "release_d"
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "package create with package name no confirm" do
    Process.put(:hex_test_package_name, :publish_package_name_no_confirm)
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")

      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["--no-progress", "--replace", "--yes"])
      assert_received {:mix_shell, :info, ["Building publish_package_name_no_confirm 0.0.1"]}

      assert {:ok, {200, _, body}} =
               Hex.API.Release.get("hexpm", "publish_package_name_no_confirm", "0.0.1")

      assert body["meta"]["app"] == "release_d"
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "publish package and docs with dry run" do
    Process.put(:hex_test_package_name, :publish_package_name_docs_dry_run)
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["--dry-run", "--yes", "--replace"])
      assert_received {:mix_shell, :info, ["Building publish_package_name_docs_dry_run 0.0.1"]}
      refute_received {:mix_shell, :info, ["Package published to" <> _]}
      refute_received {:mix_shell, :info, ["Docs published to" <> _]}
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "create with key" do
    Process.put(:hex_test_app_name, :publish_with_key)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")
      setup_auth("user2", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])
      assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "publish_with_key", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create with HEX_API_KEY" do
    Process.put(:hex_test_app_name, :publish_with_hex_api_key)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")

      send(self(), {:mix_shell_input, :prompt, "user2"})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.User.run(["key", "generate"])
      assert_received {:mix_shell, :info, ["Generating key..."]}
      assert_received {:mix_shell, :info, [key]}

      Hex.State.put(:api_key_write_unencrypted, key)
      Mix.Tasks.Hex.Publish.run(["package", "--yes", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info, ["Building publish_with_hex_api_key 0.0.1"]}

      assert {:ok, {200, _, _}} =
               Hex.API.Release.get("hexpm", "publish_with_hex_api_key", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create with an invalid HEX_API_KEY" do
    Process.put(:hex_test_app_name, :publish_with_invalid_hex_api_key)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      File.write!("myfile.txt", "hello")

      Hex.State.put(:api_key_write_unencrypted, "invalid hex api key")

      assert {:exit_code, 1} =
               ["package", "--yes", "--no-progress", "--replace"]
               |> Mix.Tasks.Hex.Publish.run()
               |> catch_throw()

      assert_received {:mix_shell, :info, ["invalid API key"]}
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create with deps" do
    Process.put(:hex_test_app_name, :publish_with_deps)
    Mix.Project.push(ReleaseDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      setup_auth("user", "hunter42")

      Mix.Tasks.Deps.Get.run([])

      error_msg = "Stopping package build due to errors.\nMissing metadata fields: links"

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])
      end

      assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "publish_with_deps", "0.0.2")
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "raise for missing metadata" do
    Process.put(:hex_test_app_name, :publish_with_missing_metadata)
    Mix.Project.push(ReleaseMeta.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")

      error_msg =
        "Stopping package build due to errors.\n" <> "Missing files: missing.txt, missing/*"

      assert_raise Mix.Error, error_msg, fn ->
        File.write!("myfile.txt", "hello")
        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])

        assert_received {:mix_shell, :info, ["Building release_c 0.0.3"]}
        assert_received {:mix_shell, :info, ["  Files:"]}
        assert_received {:mix_shell, :info, ["    myfile.txt"]}
        assert_received {:mix_shell, :info, ["  Extra: \n    c: d"]}
        refute_received {:mix_shell, :error, ["Missing metadata fields" <> _]}
      end
    end)
  after
    purge([ReleaseMeta.MixProject])
  end

  test "create with metadata" do
    Process.put(:hex_test_app_name, :publish_with_metadata)
    Mix.Project.push(ReleaseMeta.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user2", "hunter42")

      File.mkdir!("missing")
      File.write!("myfile.txt", "hello")
      File.write!("missing.txt", "hello")
      File.write!("missing/file.txt", "hello")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info, ["Building publish_with_metadata 0.0.3"]}
      assert_received {:mix_shell, :info, ["  Files:"]}
      assert_received {:mix_shell, :info, ["    myfile.txt"]}
      assert_received {:mix_shell, :info, ["  Extra: \n    c: d"]}

      assert_received {:mix_shell, :info, ["Publishing package using http://" <> _]}

      refute_received {:mix_shell, :error, ["Missing metadata fields" <> _]}
    end)
  after
    purge([ReleaseMeta.MixProject])
  end

  test "create with organization prompt" do
    Process.put(:hex_test_app_name, :publish_with_organization_prompt)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user", "hunter42")
      File.write!("myfile.txt", "hello")

      send(self(), {:mix_shell_input, :prompt, "2"})
      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info,
                       ["You are a member of one or multiple organizations. " <> _]}

      assert_received {:mix_shell, :info, ["Publishing package using http://" <> _]}
      assert_received {:mix_shell, :info, ["Transferring ownership to testorg..."]}

      assert {:ok, {200, _headers, body}} =
               Hex.API.Package.get("hexpm", "publish_with_organization_prompt")

      assert "testorg" in Enum.map(body["owners"], & &1["username"])
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create package with :organization config" do
    Process.put(:hex_test_app_name, :publish_with_org_config)
    Mix.Project.push(ReleaseRepo.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      bypass_repo("myorg")
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])

      assert_received {:mix_shell, :info,
                       ["Publishing package to private repository myorg using http://" <> _]}

      assert_received {:mix_shell, :info, ["Package published to myrepo html_url" <> _]}
    end)
  after
    purge([ReleaseRepo.MixProject])
  end

  test "create package with :organization config with no organization in user config" do
    Process.put(:hex_test_app_name, :publish_without_org_in_user_config)
    Mix.Project.push(ReleaseRepo.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      assert {:exit_code, 1} =
               ["package", "--no-progress", "--replace"]
               |> Mix.Tasks.Hex.Publish.run()
               |> catch_throw()

      refute_received {:mix_shell, :info, ["Package published to myrepo html_url" <> _]}
    end)
  after
    purge([ReleaseRepo.MixProject])
  end

  test "create package with --organization flag overrides :organization config" do
    Process.put(:hex_test_app_name, :publish_organization_flag_override)
    Mix.Project.push(ReleaseRepo.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      File.write!("mix.exs", "mix.exs")
      bypass_repo("myorg2")
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      Mix.Tasks.Hex.Publish.run([
        "package",
        "--no-progress",
        "--replace",
        "--organization",
        "myorg2"
      ])

      assert_received {:mix_shell, :info,
                       ["Publishing package to private repository myorg2 using http://" <> _]}

      assert_received {:mix_shell, :info, ["Package published to myrepo html_url" <> _]}
    end)
  after
    purge([ReleaseRepo.MixProject])
  end

  test "create with empty file list" do
    Process.put(:hex_test_app_name, :publish_with_empty_file_list)
    Mix.Project.push(ReleaseMetaNoFiles.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      setup_auth("user2", "hunter42")

      error_msg =
        "Stopping package build due to errors.\n" <>
          "Creating tarball failed: File list was empty."

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--replace"])
      end
    end)
  after
    purge([ReleaseMetaNoFiles.MixProject])
  end
end
