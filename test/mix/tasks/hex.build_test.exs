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

  defp extracted_files(path) do
    path
    |> Path.join("**")
    |> Path.wildcard(match_dot: true)
    |> Enum.reject(&File.dir?/1)
    |> Enum.map(&Path.relative_to(&1, path))
    |> Enum.sort()
  end

  defp tar_modes(name) do
    {:ok, files} = :mix_hex_erl_tar.extract(name, [:memory])
    files = Map.new(files)
    contents = {:binary, files[~c"contents.tar.gz"]}
    {:ok, entries} = :mix_hex_erl_tar.table(contents, [:compressed, :verbose])

    Map.new(entries, fn {name, _type, _size, _mtime, mode, _uid, _gid} ->
      {to_string(name), mode}
    end)
  end

  defp build_checksum(args) do
    Mix.Tasks.Hex.Build.run(args)
    assert_received {:mix_shell, :info, ["Package checksum: " <> checksum]}
    checksum
  end

  raw_file_name = Path.join(HexTest.Case.tmp_path(), <<"raw_", 0xE9>>)
  @raw_file_name_supported File.write(raw_file_name, "") == :ok
  File.rm(raw_file_name)

  test "vendored licenses accept custom LicenseRef identifiers" do
    assert :mix_hex_licenses.valid("LicenseRef-Journey")
    assert :mix_hex_licenses.valid("LicenseRef-acme.1-2")
    refute :mix_hex_licenses.valid("LicenseRef-")
    refute :mix_hex_licenses.valid("LicenseRef-Journey License")
    refute :mix_hex_licenses.valid("LicenseRef-Journey_License")
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

      assert_received {:mix_shell, :error, ["\e[33m\nYou have not included any licenses\n\e[0m"]}
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

      assert_received {:mix_shell, :error,
                       [
                         "\e[33mThe following licenses are not recognized by SPDX:\n * CustomLicense\n\nValid license identifiers are available from https://spdx.org/licenses\e[0m"
                       ]}

      assert package_created?("release_invalid_licenses-0.0.1")
    end)
  after
    purge([ReleaseInvalidLicenses.MixProject])
  end

  test "create with custom LicenseRef license" do
    Process.put(:hex_test_app_name, :release_license_ref)
    Mix.Project.push(ReleaseLicenseRef.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("myfile.txt", "hello")
      File.write!("LICENSE", "Journey License")
      Mix.Tasks.Hex.Build.run([])

      assert package_created?("release_license_ref-0.0.1")
    end)
  after
    purge([ReleaseLicenseRef.MixProject])
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
                         "\e[33mThe following licenses are not recognized by SPDX:\n * CustomLicense\n\nValid license identifiers are available from https://spdx.org/licenses\e[0m"
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
      File.write!("dir/.DS_Store", "junk")
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
      refute File.exists?("unzip/dir/.DS_Store")
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

  test "excludes junk files at any depth" do
    Process.put(:hex_test_app_name, :build_junk_files)
    Mix.Project.push(ReleaseDefaultFiles.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      junk = [
        "lib/a/.DS_Store",
        "lib/a/._foo.ex",
        "lib/a/.Spotlight-V100/Store-V2/store.db",
        "lib/a/.Trashes/501/foo.ex",
        "lib/a/.fseventsd/fseventsd-uuid",
        "lib/a/.TemporaryItems/folders.501/foo.ex",
        "lib/a/.apdisk",
        "lib/a/Icon\r",
        "lib/a/__MACOSX/lib/._foo.ex",
        "priv/x/Thumbs.db",
        "priv/x/ehthumbs.db",
        "priv/x/desktop.ini",
        "priv/y/THUMBS.DB",
        "priv/y/EhThumbs.db",
        "priv/y/Desktop.ini",
        "lib/a/#foo.ex#",
        "lib/a/foo.ex~",
        "lib/a/.foo.ex.un~",
        "lib/a/.foo.ex.swp",
        "lib/a/.foo.ex.swo",
        "lib/a/foo.ex.orig",
        "lib/a/foo.ex.rej",
        "lib/a/foo.ex.bak",
        "lib/a/.nfs0000000000c0ffee00000001",
        "lib/a/.fuse_hidden0000000100000001",
        "lib/a/.git/config",
        "lib/a/.hg/hgrc",
        "lib/a/.svn/entries",
        "lib/a/.bzr/branch-format",
        "lib/a/.jj/repo/store/type",
        "README.md~"
      ]

      Enum.each(junk, fn path ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, "junk")
      end)

      File.ln_s!("user@host.1234:1700000000", "lib/a/.#foo.ex")
      File.write!("lib/a/foo.ex", "foo")
      File.write!("lib/a/.formatter.exs", "[]")
      File.write!("priv/x/data.txt", "x")
      File.write!("priv/y/data.txt", "y")
      File.write!("README.md", "readme")

      build = Mix.Tasks.Hex.Build.prepare_package()

      assert Enum.sort(build.meta.files) == [
               "README.md",
               "lib",
               "lib/a",
               "lib/a/.formatter.exs",
               "lib/a/foo.ex",
               "priv",
               "priv/x",
               "priv/x/data.txt",
               "priv/y",
               "priv/y/data.txt"
             ]

      Mix.Tasks.Hex.Build.run([])
      extract("build_junk_files-0.0.1.tar", "unzip")

      assert extracted_files("unzip") == [
               "README.md",
               "lib/a/.formatter.exs",
               "lib/a/foo.ex",
               "priv/x/data.txt",
               "priv/y/data.txt"
             ]
    end)
  after
    purge([ReleaseDefaultFiles.MixProject])
  end

  test "excludes junk files named by file patterns" do
    Process.put(:hex_test_app_name, :build_named_junk_files)
    Mix.Project.push(ReleaseAllFiles.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.write!("myfile.txt~", "backup")
      File.write!("Thumbs.db", "junk")
      File.mkdir_p!("__MACOSX/dir")
      File.write!("__MACOSX/dir/myfile.txt", "junk")

      build = Mix.Tasks.Hex.Build.prepare_package()
      assert build.meta.files == ["myfile.txt"]
    end)
  after
    purge([ReleaseAllFiles.MixProject])
  end

  test "includes junk files listed without wildcards" do
    Process.put(:hex_test_app_name, :build_literal_junk_files)
    Mix.Project.push(ReleaseLiteralJunk.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.mkdir!("priv")
      File.write!("priv/data.txt", "data")
      File.write!("priv/patch.orig", "fixture")
      File.write!("priv/other.orig", "junk")
      File.write!("priv/data.bak", "junk")

      build = Mix.Tasks.Hex.Build.prepare_package()

      assert Enum.sort(build.meta.files) ==
               ["myfile.txt", "priv", "priv/data.txt", "priv/patch.orig"]
    end)
  after
    purge([ReleaseLiteralJunk.MixProject])
  end

  test "excludes build outputs" do
    Process.put(:hex_test_app_name, :build_outputs)
    Mix.Project.push(ReleaseAllFiles.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.mkdir_p!("_build/test/lib/build_outputs/ebin")
      File.write!("_build/test/lib/build_outputs/ebin/build_outputs.app", "app")
      File.mkdir_p!("_build/dev/lib/build_outputs/ebin")
      File.write!("_build/dev/lib/build_outputs/ebin/build_outputs.app", "app")
      File.mkdir_p!("deps/dep")
      File.write!("deps/dep/mix.exs", "dep")

      Mix.Tasks.Hex.Build.run(["--unpack"])
      assert File.dir?("build_outputs-0.0.1")

      checksums =
        Enum.map(
          [[], [], [], ["-o", "custom.tar"], ["-o", "custom.tar"]],
          &build_checksum/1
        )

      assert [_checksum] = Enum.uniq(checksums)

      build = Mix.Tasks.Hex.Build.prepare_package(output: "custom.tar")
      assert build.meta.files == ["myfile.txt"]

      build = Mix.Tasks.Hex.Build.prepare_package()
      assert Enum.sort(build.meta.files) == ["custom.tar", "myfile.txt"]

      build = Mix.Tasks.Hex.Build.prepare_package(output: "CUSTOM.tar")

      if File.exists?("CUSTOM.tar") do
        assert build.meta.files == ["myfile.txt"]
      else
        assert Enum.sort(build.meta.files) == ["custom.tar", "myfile.txt"]
      end

      extract("build_outputs-0.0.1.tar", "unzip")
      assert extracted_files("unzip") == ["myfile.txt"]
    end)
  after
    purge([ReleaseAllFiles.MixProject])
  end

  test "literal file patterns match file names exactly" do
    Process.put(:hex_test_app_name, :build_case_mismatch)
    Mix.Project.push(ReleaseCaseMismatch.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.write!("LICENSE", "license")
      File.mkdir!("dir")
      File.write!("dir/a.txt", "a")

      build = Mix.Tasks.Hex.Build.prepare_package()
      assert build.meta.files == ["myfile.txt"]

      error_msg = "Stopping package build due to errors.\nMissing files: License, Dir/*.txt"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseCaseMismatch.MixProject])
  end

  if not @raw_file_name_supported do
    @tag skip: "file system rejects file names that are not valid UTF-8"
  end

  test "errors when a file name is not valid UTF-8" do
    Process.put(:hex_test_app_name, :build_invalid_file_name)
    Mix.Project.push(ReleaseAllFiles.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.mkdir!("dir")
      nested_path = <<"dir/caf", 0xE9, ".txt">>
      File.write!(nested_path, "")

      error_msg = ~S(Can't build package when file name is not valid UTF-8: "dir/caf\xE9.txt")

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.prepare_package()
      end

      File.rm!(nested_path)
      root_path = <<"caf", 0xE9, ".txt">>
      File.write!(root_path, "")

      error_msg = ~S(Can't build package when file name is not valid UTF-8: "caf\xE9.txt")

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.prepare_package()
      end

      File.rm!(root_path)
    end)
  after
    purge([ReleaseAllFiles.MixProject])
  end

  test "converts file names to paths in the native encoding" do
    assert Mix.Tasks.Hex.Build.disk_path("lib/café.ex", :utf8) == ~c"lib/café.ex"
    assert Mix.Tasks.Hex.Build.disk_path("lib/café.ex", :latin1) == ~c"lib/cafÃ©.ex"

    assert Mix.Tasks.Hex.Build.tar_files(["lib/日本.ex"], nil, :utf8) == [
             {"lib/日本.ex", ~c"lib/日本.ex"}
           ]

    assert Mix.Tasks.Hex.Build.tar_files(["lib/日本.ex"], nil, :latin1) == [
             {"lib/日本.ex", ~c"lib/" ++ [0xE6, 0x97, 0xA5, 0xE6, 0x9C, 0xAC] ++ ~c".ex"}
           ]

    assert Mix.Tasks.Hex.Build.tar_files(["a.sh", "b"], MapSet.new(["a.sh"]), :utf8) == [
             {"a.sh", ~c"a.sh", %{executable: true}},
             {"b", ~c"b", %{executable: false}}
           ]
  end

  if Code.ensure_loaded?(:peer) do
    test "expands file patterns the same with the latin1 native name encoding" do
      in_tmp(fn ->
        File.mkdir_p!("lib/sub/日本")
        File.mkdir_p!("lib/.hidden")
        File.write!("lib/café.ex", "")
        File.write!("lib/sub/naïve.ex", "")
        File.write!("lib/sub/日本/a.ex", "")
        File.write!("lib/.hidden/b.ex", "")

        patterns = %{
          "lib/caf?.ex" => ["lib/café.ex"],
          "lib/caf[é].ex" => ["lib/café.ex"],
          "lib/{café,none}.ex" => ["lib/café.ex"],
          "lib/**/*.ex" => ["lib/café.ex", "lib/sub/naïve.ex", "lib/sub/日本/a.ex"],
          "lib/*/na?ve.ex" => ["lib/sub/naïve.ex"],
          "lib/sub/??/*" => ["lib/sub/日本/a.ex"],
          "lib/sub/日本/a.ex" => ["lib/sub/日本/a.ex"]
        }

        Enum.each(patterns, fn {pattern, expected} ->
          assert Hex.Wildcard.wildcard(pattern) == expected
        end)

        paths = Enum.flat_map(:code.get_path(), &[~c"-pa", &1])

        {:ok, peer, _node} =
          :peer.start_link(%{args: [~c"+fnl" | paths], connection: :standard_io})

        try do
          assert :peer.call(peer, :file, :native_name_encoding, []) == :latin1
          :ok = :peer.call(peer, File, :cd!, [File.cwd!()])

          Enum.each(patterns, fn {pattern, expected} ->
            assert :peer.call(peer, Hex.Wildcard, :wildcard, [pattern]) == expected
          end)
        after
          :peer.stop(peer)
        end
      end)
    end
  end

  test "sets executable files with the executables option" do
    Process.put(:hex_test_app_name, :build_executables)

    build = fn executables ->
      Process.put(:hex_test_executables, executables)
      Mix.Project.push(ReleaseExecutables.MixProject)

      try do
        Mix.Tasks.Hex.Build.run([])
        tar_modes("build_executables-0.0.1.tar")
      after
        Mix.Project.pop()
      end
    end

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.write!("run.sh", "run")
      File.chmod!("run.sh", 0o755)
      File.mkdir!("bin")
      File.write!("bin/tool", "tool")
      File.chmod!("bin/tool", 0o644)
      File.write!("bin/other", "other")
      File.chmod!("bin/other", 0o755)
      File.write!("other.sh", "other")

      assert build.(nil) == %{
               "bin/other" => 0o100755,
               "bin/tool" => 0o100644,
               "myfile.txt" => 0o100644,
               "run.sh" => 0o100755
             }

      assert build.(["bin"]) == %{
               "bin/other" => 0o100755,
               "bin/tool" => 0o100755,
               "myfile.txt" => 0o100644,
               "run.sh" => 0o100644
             }

      assert build.(["*.txt", "bin/t*"]) == %{
               "bin/other" => 0o100644,
               "bin/tool" => 0o100755,
               "myfile.txt" => 0o100755,
               "run.sh" => 0o100644
             }

      error_msg =
        "Stopping package build due to errors.\nExecutables not in package: other.sh, missing/*"

      assert_raise Mix.Error, error_msg, fn ->
        build.(["run.sh", "other.sh", "missing/*"])
      end
    end)
  after
    Process.delete(:hex_test_executables)
    purge([ReleaseExecutables.MixProject])
  end

  test "errors when package file escapes project root" do
    Process.put(:hex_test_app_name, :build_with_escaping_files)
    Mix.Project.push(ReleaseEscapingFiles.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("../../README.md", "outside")
      outside_readme = Path.expand("../../README.md")

      error_msg = "Creating tarball failed: unsafe path in tarball: #{outside_readme}"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseEscapingFiles.MixProject])
  end

  test "errors when package symlink escapes project root" do
    Process.put(:hex_test_app_name, :build_with_escaping_symlink)
    Mix.Project.push(ReleaseEscapingSymlink.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("../../README.md", "outside")
      File.ln_s!("../../README.md", "README.md")

      error_msg =
        "Creating tarball failed: unsafe symlink in tarball: README.md -> ../../README.md"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseEscapingSymlink.MixProject])
  end

  test "errors when package file resolves through escaping symlink directory" do
    Process.put(:hex_test_app_name, :build_with_escaping_symlink_directory)
    Mix.Project.push(ReleaseEscapingSymlinkDirectory.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.mkdir!("../outside")
      File.write!("../outside/secret.txt", "outside")
      File.ln_s!("../outside", "link")

      error_msg = "Creating tarball failed: unsafe path in tarball: link/secret.txt"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseEscapingSymlinkDirectory.MixProject])
  end

  test "preserves package symlink resolving inside project root" do
    Process.put(:hex_test_app_name, :build_with_internal_symlink)
    Mix.Project.push(ReleaseInternalSymlink.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())
      File.write!("README.md", "inside")
      File.mkdir!("dir")
      File.ln_s!("../README.md", "dir/link")

      Mix.Tasks.Hex.Build.run([])
      extract("build_with_internal_symlink-0.0.1.tar", "unzip")

      assert File.lstat!("unzip/dir/link").type == :symlink
      assert File.read_link!("unzip/dir/link") == "../README.md"
    end)
  after
    purge([ReleaseInternalSymlink.MixProject])
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

  test "create with secret_scan" do
    Process.put(:hex_test_app_name, :build_secret_scan)
    Mix.Project.push(ReleaseSecretScan.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      Mix.Tasks.Hex.Build.run(["--unpack"])

      # A keyword list has to print like a map does, not blow up on the tuples.
      assert_received {:mix_shell, :info, ["  Secret scan: \n    ignore: test/fixtures/**"]}

      {:ok, metadata} = :file.consult("build_secret_scan-0.0.1/hex_metadata.config")
      assert {"secret_scan", [{"ignore", ["test/fixtures/**"]}]} in metadata
    end)
  after
    purge([ReleaseSecretScan.MixProject])
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

      assert_received {:mix_shell, :error, [^message]}
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

  test "errors with umbrella deps" do
    Process.put(:hex_test_app_name, :includes_umbrella_deps)
    Mix.Project.push(ReleaseInUmbrellaDeps.MixProject)

    in_tmp(fn ->
      Hex.State.put(:cache_home, tmp_path())

      File.write!("myfile.txt", "hello")
      File.chmod!("myfile.txt", 0o100644)

      error_msg =
        "Stopping package build due to errors.\nDependencies excluded from the package (only Hex packages can be dependencies): ecto"

      assert_raise Mix.Error, error_msg, fn ->
        Mix.Tasks.Hex.Build.run([])
      end
    end)
  after
    purge([ReleaseInUmbrellaDeps.MixProject])
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
