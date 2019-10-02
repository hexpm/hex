defmodule Mix.Tasks.Docs do
  def run(["--canonical", "https://hexdocs.pm/invalid_filename"]) do
    File.mkdir_p!("doc")
    File.write!("doc/index.html", "the index")
    File.write!("doc/1.5.5", "semver file")
  end

  def run(["--canonical", "https://hexdocs.pm/invalid_dirname"]) do
    File.mkdir_p!("doc")
    File.write!("doc/index.html", "the index")
    File.mkdir_p!("doc/1.5.5")
    File.write!("doc/1.5.5/index.html", "sub index")
  end

  def run(_) do
    File.mkdir_p!("doc")
    File.write!("doc/index.html", "the index")
  end
end

defmodule Mix.Tasks.Hex.PublishTest do
  use HexTest.Case
  @moduletag :integration

  defmodule DocsSimple.MixProject do
    def project do
      [app: :ex_doc, version: "0.1.0"]
    end
  end

  defmodule DocsError.MixProject do
    def project do
      [app: :ex_doc, version: "0.1.1"]
    end
  end

  defmodule DocsFilenameError.MixProject do
    def project do
      [app: :invalid_filename, version: "0.1.0"]
    end
  end

  defmodule DocsDirnameError.MixProject do
    def project do
      [app: :invalid_dirname, version: "0.1.0"]
    end
  end

  test "ensure user exists" do
    Mix.Project.push(ReleaseSimple.MixProject)
    Hex.State.put(:home, tmp_path("does_not_exist"))

    in_tmp(fn ->
      send(self(), {:mix_shell_input, :yes?, false})

      assert_raise Mix.Error, "No authenticated user found. Run `mix hex.user auth`", fn ->
        Mix.Tasks.Hex.Publish.run([])
      end
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create and revert a package" do
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user2", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

      message =
        "Package published to http://localhost:4043/packages/release_a/0.0.1 " <>
          "(a7ba7cb9e060d43b32ec48df731522c3ae8bd4bbf0b00db0b8eca106da273b63)"

      assert_received {:mix_shell, :info, [^message]}

      assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "release_a", "0.0.1")

      message =
        "Before publishing, please read the Code of Conduct: https://hex.pm/policies/codeofconduct\n"

      assert_received {:mix_shell, :info, [^message]}

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--revert", "0.0.1"])
      assert {:ok, {404, _, _}} = Hex.API.Release.get("hexpm", "release_a", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create a package without confirming" do
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user2", "hunter42")
      # note that we don't need the yes? input here
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--yes"])
      assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "release_a", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create and revert docs" do
    Mix.Project.push(DocsSimple.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress"])
      assert_received {:mix_shell, :info, ["Docs published to https://hexdocs.pm/ex_doc/0.1.0"]}

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--revert", "0.1.0"])
      assert_received {:mix_shell, :info, ["Reverted docs for ex_doc 0.1.0"]}
    end)
  end

  test "publish docs with invalid filename" do
    Mix.Project.push(DocsFilenameError.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      error_msg = "Invalid filename: top-level filenames cannot match a semantic version pattern."

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["docs", "--no-progress"])

        refute_received {:mix_shell, :info,
                         ["Docs published to https://hexdocs.pm/invalid_filename/0.1.0"]}
      end
    end)
  end

  test "publish docs with invalid dirname" do
    Mix.Project.push(DocsDirnameError.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      error_msg = "Invalid filename: top-level filenames cannot match a semantic version pattern."

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["docs", "--no-progress"])

        refute_received {:mix_shell, :info,
                         ["Docs published to https://hexdocs.pm/invalid_dirname/0.1.0"]}
      end
    end)
  end

  test "docs when package is not published yet" do
    Mix.Project.push(DocsError.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress"])

      message = "Publishing docs failed due to the package not being published yet"
      assert_received {:mix_shell, :error, [^message]}

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["docs", "--revert", "0.1.1"])
      assert_received {:mix_shell, :info, ["Docs do not exist"]}
    end)
  end

  test "package create with package name" do
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user2", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})

      assert_raise Mix.Error, ~r"Invalid arguments", fn ->
        Mix.Tasks.Hex.Publish.run(["invalid", "--no-progress"])
      end

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["--no-progress"])
      message = "Building released_name 0.0.1"
      assert_received {:mix_shell, :info, [^message]}

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      assert {:ok, {200, body, _}} = Hex.API.Release.get("hexpm", "released_name", "0.0.1")
      assert body["meta"]["app"] == "release_d"
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "package create with package name no confirm" do
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["--no-progress", "--yes"])
      message = "Building released_name 0.0.1"
      assert_received {:mix_shell, :info, [^message]}

      assert {:ok, {200, body, _}} = Hex.API.Release.get("hexpm", "released_name", "0.0.1")
      assert body["meta"]["app"] == "release_d"
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "publish package and docs with dry run" do
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["--dry-run", "--yes"])
      message = "Building released_name 0.0.1"
      assert_received {:mix_shell, :info, [^message]}
      refute_received {:mix_shell, :info, ["Package published to" <> _]}
      refute_received {:mix_shell, :info, ["Docs published to" <> _]}
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "create with key" do
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user2", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "release_a", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create with HEX_API_KEY" do
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      send(self(), {:mix_shell_input, :prompt, "user2"})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.User.run(["key", "generate"])
      assert_received {:mix_shell, :info, ["Generating key..."]}
      assert_received {:mix_shell, :info, [key]}

      Hex.State.put(:api_key_write_unencrypted, key)
      Mix.Tasks.Hex.Publish.run(["package", "--yes", "--no-progress"])

      message = "Building release_a 0.0.1"
      assert_received {:mix_shell, :info, [^message]}
      assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "release_a", "0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create with deps" do
    Mix.Project.push(ReleaseDeps.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      Mix.Tasks.Deps.Get.run([])

      error_msg = "Stopping package build due to errors.\nMissing metadata fields: links"

      assert_raise Mix.Error, error_msg, fn ->
        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

        assert_received {:mix_shell, :error, ["No files"]}
        assert {:ok, {200, _, _}} = Hex.API.Release.get("hexpm", "release_b", "0.0.2")
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "raise for missing metadata" do
    Mix.Project.push(ReleaseMeta.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      error_msg =
        "Stopping package build due to errors.\n" <> "Missing files: missing.txt, missing/*"

      assert_raise Mix.Error, error_msg, fn ->
        File.write!("myfile.txt", "hello")
        send(self(), {:mix_shell_input, :yes?, true})
        send(self(), {:mix_shell_input, :prompt, "hunter42"})
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

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
    Mix.Project.push(ReleaseMeta.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user2", "hunter42")

      File.mkdir!("missing")
      File.write!("myfile.txt", "hello")
      File.write!("missing.txt", "hello")
      File.write!("missing/file.txt", "hello")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

      assert_received {:mix_shell, :info, ["Building release_c 0.0.3"]}
      assert_received {:mix_shell, :info, ["  Files:"]}
      assert_received {:mix_shell, :info, ["    myfile.txt"]}
      assert_received {:mix_shell, :info, ["  Extra: \n    c: d"]}

      message = "Publishing package to public repository hexpm."
      assert_received {:mix_shell, :info, [^message]}
      refute_received {:mix_shell, :error, ["Missing metadata fields" <> _]}
    end)
  after
    purge([ReleaseMeta.MixProject])
  end

  test "create package with :organization config" do
    Mix.Project.push(ReleaseRepo.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      bypass_repo("myorg")
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

      message = "Publishing package to private repository myorg."
      assert_received {:mix_shell, :info, [^message]}

      assert_received {:mix_shell, :info, ["Package published to myrepo html_url" <> _]}
    end)
  after
    purge([ReleaseRepo.MixProject])
  end

  test "create package with :organization config with no organization in user config" do
    Mix.Project.push(ReleaseRepo.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
    end)
  after
    purge([ReleaseRepo.MixProject])
  end

  test "create package with --organization flag overrides :organization config" do
    Mix.Project.push(ReleaseRepo.MixProject)

    in_tmp(fn ->
      Hex.State.put(:home, tmp_path())
      bypass_repo("myorg2")
      setup_auth("user", "hunter42")

      send(self(), {:mix_shell_input, :yes?, true})
      send(self(), {:mix_shell_input, :prompt, "hunter42"})
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress", "--organization", "myorg2"])

      message = "Publishing package to private repository myorg2."
      assert_received {:mix_shell, :info, [^message]}

      assert_received {:mix_shell, :info, ["Package published to myrepo html_url" <> _]}
    end)
  after
    purge([ReleaseRepo.MixProject])
  end
end
