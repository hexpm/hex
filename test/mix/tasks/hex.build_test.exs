defmodule Mix.Tasks.Hex.BuildTest do
  use HexTest.IntegrationCase

  defp package_created?(name) do
    File.exists?("#{name}.tar")
  end

  defp extract(name, path) do
    {:ok, files} = :mix_hex_erl_tar.extract(name, [:memory])
    files = Map.new(files)

    :ok =
      :mix_hex_erl_tar.extract({:binary, files[~c"contents.tar.gz"]}, [:compressed, cwd: path])
  end

  test "create" do
    Process.put(:hex_test_app_name, :build_app_name)
    Mix.Project.push(ReleaseSimple.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      Mix.Tasks.Hex.Build.run([])
      assert package_created?("build_app_name-0.0.1")
    end)
  after
    purge([ReleaseSimple.MixProject])
  end

  test "create with missing licenses" do
    Process.put(:hex_test_app_name, :release_missing_licenses)
    Mix.Project.push(ReleaseMissingLicenses.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info, ["\e[33m\nYou have not included any licenses\n\e[0m"]}
      assert package_created?("release_missing_licenses-0.0.1")
    end)
  after
    purge([ReleaseMissingLicenses.MixProject])
  end

  test "create with invalid licenses" do
    Process.put(:hex_test_app_name, :release_invalid_licenses)
    Mix.Project.push(ReleaseInvalidLicenses.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Build.run([])

      assert_received {:mix_shell, :info,
                       [
                         "\e[33mThe following licenses are not recognized by SPDX:\n * CustomLicense\n\nConsider using licenses from https://spdx.org/licenses\e[0m"
                       ]}

      assert package_created?("release_invalid_licenses-0.0.1")
    end)
  after
    purge([ReleaseInvalidLicenses.MixProject])
  end

  test "create private package with invalid licenses" do
    Process.put(:hex_test_app_name, :release_repo_invalid_licenses)
    Mix.Project.push(ReleaseRepoInvalidLicenses.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("myfile.txt", "hello")
      Mix.Tasks.Hex.Build.run([])

      refute_received {:mix_shell, :info,
                       [
                         "\e[33m\nYou have chosen 1 or more licenses that are not recognized by SPDX\nConsider using a license from https://spdx.org/licenses/\n\e[0m"
                       ]}

      assert package_created?("release_repo_invalid_licenses-0.0.1")
    end)
  after
    purge([ReleaseRepoInvalidLicenses.MixProject])
  end

  test "create with package name" do
    Process.put(:hex_test_package_name, :build_package_name)
    Mix.Project.push(ReleaseName.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      Mix.Tasks.Hex.Build.run([])
      assert package_created?("build_package_name-0.0.1")
    end)
  after
    purge([ReleaseName.MixProject])
  end

  test "create with files" do
    Process.put(:hex_test_app_name, :build_with_files)
    Mix.Project.push(ReleaseFiles.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.mkdir!("dir")
      File.mkdir!("empty_dir")
      File.write!("dir/.dotfile", "")
      File.ln_s("dir2", "dir/a_link_to_dir2")
      File.mkdir!("dir/dir2")
      File.ln_s("empty_dir", "link_dir")

      # mtime_dir = File.stat!("dir").mtime
      mtime_empty_dir = File.stat!("empty_dir").mtime
      mtime_file = File.stat!("dir/.dotfile").mtime
      mtime_link = File.stat!("link_dir").mtime

      File.write!("myfile.txt", "hello")
      File.write!("executable.sh", "world")
      File.write!("dir/dir2/test.txt", "and")
      File.chmod!("myfile.txt", 0o100644)
      File.chmod!("executable.sh", 0o100755)
      File.chmod!("dir/dir2/test.txt", 0o100644)

      Mix.Tasks.Hex.Build.run([])

      extract("build_with_files-0.0.1.tar", "unzip")

      # Check that mtimes are not retained for files and directories and symlinks
      # erl_tar does not set mtime from tar if a directory contain files
      # assert File.stat!("unzip/dir").mtime != mtime_dir
      assert File.stat!("unzip/empty_dir").mtime != mtime_empty_dir
      assert File.stat!("unzip/dir/.dotfile").mtime != mtime_file
      assert File.stat!("unzip/link_dir").mtime != mtime_link

      assert File.lstat!("unzip/link_dir").type == :symlink
      assert File.lstat!("unzip/dir/a_link_to_dir2").type == :symlink
      assert File.lstat!("unzip/empty_dir").type == :directory
      assert File.read!("unzip/myfile.txt") == "hello"
      assert File.read!("unzip/dir/.dotfile") == ""
      assert File.read!("unzip/dir/dir2/test.txt") == "and"
      assert File.stat!("unzip/myfile.txt").mode == 0o100644
      assert File.stat!("unzip/executable.sh").mode == 0o100755
    end)
  after
    purge([ReleaseFiles.MixProject])
  end

  test "create with excluded files" do
    Process.put(:hex_test_app_name, :build_with_excluded_files)
    Mix.Project.push(ReleaseExcludePatterns.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.write!("exclude.txt", "world")
      File.chmod!("myfile.txt", 0o100644)
      File.chmod!("exclude.txt", 0o100644)

      Mix.Tasks.Hex.Build.run([])

      extract("build_with_excluded_files-0.0.1.tar", "unzip")

      assert File.ls!("unzip/") == ["myfile.txt"]
      assert File.read!("unzip/myfile.txt") == "hello"
      assert File.stat!("unzip/myfile.txt").mode == 0o100644
    end)
  after
    purge([ReleaseExcludePatterns.MixProject])
  end

  test "create with custom output path" do
    Process.put(:hex_test_app_name, :build_custom_output_path)
    Mix.Project.push(Sample.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("mix.exs", "mix.exs")
      File.chmod!("mix.exs", 0o100644)

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      Mix.Tasks.Hex.Build.run(["-o", "custom.tar"])

      assert File.exists?("custom.tar")
    end)
  after
    purge([Sample.MixProject])
  end

  test "create with deps" do
    Process.put(:hex_test_app_name, :build_with_deps)
    Mix.Project.push(ReleaseDeps.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      Mix.Tasks.Deps.Get.run([])

      error_msg = "Stopping package build due to errors.\nMissing metadata fields: links"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :error, ["No files"]}
        refute package_created?("release_b-0.0.2")
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  # TODO: convert to integration test
  test "create with custom repo deps" do
    Process.put(:hex_test_app_name, :build_with_custom_repo_deps)
    Mix.Project.push(ReleaseCustomRepoDeps.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      build = Mix.Tasks.Hex.Build.prepare_package()

      assert [
               %{name: "ex_doc", repository: "hexpm"},
               %{name: "ecto", repository: "my_repo"}
             ] = build.meta.requirements
    end)
  after
    purge([ReleaseCustomRepoDeps.MixProject])
  end

  test "errors when there is a git dependency" do
    Process.put(:hex_test_app_name, :build_git_dependency)
    Mix.Project.push(ReleaseGitDeps.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg =
        "Stopping package build due to errors.\n" <>
          "Dependencies excluded from the package (only Hex packages can be dependencies): ecto, gettext"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseGitDeps.MixProject])
  end

  test "errors with app false dependency" do
    Process.put(:hex_test_app_name, :build_app_false_dependency)
    Mix.Project.push(ReleaseAppFalseDep.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg = "Can't build package when :app is set for dependency ex_doc, remove `app: ...`"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseAppFalseDep.MixProject])
  end

  test "create with meta" do
    Process.put(:hex_test_app_name, :build_with_meta)
    Mix.Project.push(ReleaseMeta.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg =
        "Stopping package build due to errors.\n" <> "Missing files: missing.txt, missing/*"

      assert_raise Mix.Error, error_msg, fn ->
        File.write!("myfile.txt", "hello")
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :info, ["Building release_c 0.0.3"]}
        assert_received {:mix_shell, :info, ["  Files:"]}
        assert_received {:mix_shell, :info, ["    myfile.txt"]}
      end
    end)
  after
    purge([ReleaseMeta.MixProject])
  end

  test "reject package if description is missing" do
    Process.put(:hex_test_app_name, :build_no_description)
    Mix.Project.push(ReleaseNoDescription.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg =
        "Stopping package build due to errors.\n" <>
          "Missing metadata fields: description, licenses, links"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])

        assert_received {:mix_shell, :info, ["Building release_e 0.0.1"]}

        refute package_created?("release_e-0.0.1")
      end
    end)
  after
    purge([ReleaseNoDescription.MixProject])
  end

  test "error if description is too long" do
    Process.put(:hex_test_app_name, :build_too_long_description)
    Mix.Project.push(ReleaseTooLongDescription.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg =
        "Stopping package build due to errors.\n" <>
          "Missing metadata fields: licenses, links\n" <>
          "Package description is too long (exceeds 300 characters)"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseTooLongDescription.MixProject])
  end

  test "error if misspelled organization" do
    Process.put(:hex_test_app_name, :build_misspelled_organization)
    Mix.Project.push(ReleaseMisspelledOrganization.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg = "Invalid Hex package config :organisation, use spelling :organization"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseMisspelledOrganization.MixProject])
  end

  test "warn if misplaced config" do
    Process.put(:hex_test_app_name, :build_warn_config_location)
    Mix.Project.push(ReleaseOrganizationWrongLocation.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      Mix.Tasks.Hex.Build.run([])
      assert_received {:mix_shell, :info, ["Building build_warn_config_location 0.0.1"]}

      message =
        "\e[33mMix project configuration :organization belongs under the :package key, " <>
          "did you misplace it?\e[0m"

      assert_received {:mix_shell, :info, [^message]}
    end)
  after
    purge([ReleaseOrganizationWrongLocation.MixProject])
  end

  test "error if hex_metadata.config is included" do
    Process.put(:hex_test_app_name, :build_reserved_file)
    Mix.Project.push(ReleaseIncludeReservedFile.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      error_msg =
        "Stopping package build due to errors.\n" <>
          "Do not include this file: hex_metadata.config"

      assert_raise Mix.Error, error_msg, fn ->
        File.write!("hex_metadata.config", "hello")
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseIncludeReservedFile.MixProject])
  end

  test "build and unpack" do
    Process.put(:hex_test_app_name, :build_and_unpack)
    Mix.Project.push(Sample.MixProject)

    in_fixture("sample", fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      Mix.Tasks.Hex.Build.run(["--unpack"])
      assert_received({:mix_shell, :info, ["Saved to build_and_unpack-0.0.1"]})

      assert File.exists?("build_and_unpack-0.0.1/mix.exs")
      assert File.exists?("build_and_unpack-0.0.1/hex_metadata.config")

      Mix.Tasks.Hex.Build.run(["--unpack", "-o", "custom"])
      assert_received({:mix_shell, :info, ["Saved to custom"]})

      assert File.exists?("custom/mix.exs")
      assert File.exists?("custom/hex_metadata.config")
    end)
  after
    purge([Sample.MixProject])
  end
end
