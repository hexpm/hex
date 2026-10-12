defmodule Mix.Tasks.Hex.PackageTest do
  use HexTest.IntegrationCase

  @git_diff_command "git diff --no-index --no-color --no-ext-diff __PATH1__ __PATH2__"

  defp put_registry_outer_checksum(package, version, checksum) do
    Hex.Registry.Server.open()
    Hex.Registry.Server.prefetch([{"hexpm", package}])
    {:ok, _versions} = Hex.Registry.Server.versions("hexpm", package)

    :sys.replace_state(Hex.Registry.Server, fn state ->
      :ets.insert(state.ets, {{:outer_checksum, "hexpm", package, version}, checksum})
      state
    end)
  end

  defp in_diff_fixture(fun) do
    in_fixture("diff", fn ->
      Mix.Project.push(ReleaseDeps.MixProject)
      Mix.Dep.Lock.write(%{ex_doc: {:hex, :ex_doc, "0.0.1"}})
      set_home_cwd()
      Mix.Task.run("deps.get")
      fun.()
    end)
  end

  test "fetch: success" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "0.0.1"])
      msg = "ex_doc v0.0.1 downloaded to #{cwd}/ex_doc-0.0.1.tar"
      assert_received {:mix_shell, :info, [^msg]}
      assert File.exists?("#{cwd}/ex_doc-0.0.1.tar")
      refute File.exists?("#{cwd}/ex_doc-0.0.1/")
    end)
  end

  test "fetch: success using latest version" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Mix.Tasks.Hex.Package.run(["fetch", "postgrex"])
      msg = "postgrex v0.2.1 downloaded to #{cwd}/postgrex-0.2.1.tar"
      assert_received {:mix_shell, :info, [^msg]}
      assert File.exists?("#{cwd}/postgrex-0.2.1.tar")
      refute File.exists?("#{cwd}/postgrex-0.2.1/")
    end)
  end

  test "fetch: custom repo" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "0.0.1", "--repo", "hexpm"])
      msg = "ex_doc v0.0.1 downloaded to #{cwd}/ex_doc-0.0.1.tar"
      assert_received {:mix_shell, :info, [^msg]}
      assert File.exists?("#{cwd}/ex_doc-0.0.1.tar")
    end)
  end

  test "fetch: to folder" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "--output", "#{cwd}/test", "0.0.1"])
      msg = "ex_doc v0.0.1 downloaded to #{cwd}/test/ex_doc-0.0.1.tar"
      assert_received {:mix_shell, :info, [^msg]}
      assert File.exists?("#{cwd}/test/ex_doc-0.0.1.tar")
    end)
  end

  test "fetch: to folder unpack" do
    in_tmp(fn ->
      cwd = File.cwd!()

      Mix.Tasks.Hex.Package.run([
        "fetch",
        "ex_doc",
        "--output",
        "#{cwd}/test",
        "--unpack",
        "0.0.1"
      ])

      msg = "ex_doc v0.0.1 extracted to #{cwd}/test"
      assert_received {:mix_shell, :info, [^msg]}
      assert File.exists?("#{cwd}/test") && File.dir?("#{cwd}/test")
    end)
  end

  # TODO: add `capture_bin_io/2`.
  # test "fetch: to stdout" do
  #   in_tmp(fn ->
  #     tarball =
  #       capture_io(fn ->
  #         Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "--output", "-", "0.0.1"])
  #       end)

  #     Hex.Tar.unpack!({:binary, tarball}, :memory)
  #   end)
  # end

  test "fetch: to folder rejects a tarball that doesn't match the registry checksum" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Hex.State.put(:cache_home, cwd)
      put_registry_outer_checksum("ex_doc", "0.0.1", <<0::256>>)

      assert_raise Mix.Error, ~r/Checksum mismatch against registry \(outer\)/, fn ->
        Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "--output", "#{cwd}/test", "0.0.1"])
      end

      refute File.exists?("#{cwd}/test/ex_doc-0.0.1.tar")
    end)
  end

  test "fetch: to stdout rejects a tarball that doesn't match the registry checksum" do
    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      put_registry_outer_checksum("ex_doc", "0.0.1", <<0::256>>)

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          assert_raise Mix.Error, ~r/Checksum mismatch against registry \(outer\)/, fn ->
            Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "--output", "-", "0.0.1"])
          end
        end)

      assert output == ""
    end)
  end

  test "fetch: to stdout with unpack flag" do
    assert_raise Mix.Error,
                 ~r"Cannot unpack the package while output destination is stdout",
                 fn ->
                   Mix.Tasks.Hex.Package.run([
                     "fetch",
                     "ex_doc",
                     "--output",
                     "-",
                     "--unpack",
                     "0.0.1"
                   ])
                 end
  end

  test "fetch: package not found" do
    assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
      Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "2.0.0"])
    end
  end

  test "diff: success with version number" do
    in_diff_fixture(fn ->
      Hex.State.put(:diff_command, @git_diff_command)

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.1.0"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ ~s(-{<<"version">>,<<"0.0.1">>}.)
      assert out =~ ~s(+{<<"version">>,<<"0.1.0">>}.)
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: outdated lockfile with single version number" do
    msg = "Can't continue due to errors on dependencies"

    in_diff_fixture(fn ->
      assert_raise Mix.Error, msg, fn ->
        Mix.Dep.Lock.write(%{
          ok: {:ex_doc, "https://github.com/elixir-lang/ex_doc.git", "abcdefghi", []}
        })

        Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "1.0.0"])
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: not having target package with single version number" do
    msg =
      "Cannot find the app \"tesla\" in \"mix.lock\" file, " <>
        "please ensure it has been specified in \"mix.exs\" and run \"mix deps.get\""

    in_diff_fixture(fn ->
      assert_raise Mix.Error, msg, fn ->
        Mix.Tasks.Hex.Package.run(["diff", "tesla", "1.0.0"])
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: success with version range" do
    in_diff_fixture(fn ->
      Hex.State.put(:diff_command, @git_diff_command)

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..0.1.0"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ ~s(-{<<"version">>,<<"0.0.1">>}.)
      assert out =~ ~s(+{<<"version">>,<<"0.1.0">>}.)
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: success (variant args)" do
    in_diff_fixture(fn ->
      Hex.State.put(:diff_command, @git_diff_command)

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1", "0.1.0"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ ~s(-{<<"version">>,<<"0.0.1">>}.)
      assert out =~ ~s(+{<<"version">>,<<"0.1.0">>}.)
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: custom diff command" do
    in_diff_fixture(fn ->
      Hex.State.put(:diff_command, "ls __PATH1__ __PATH2__")

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..0.1.0"])) ==
               {:exit_code, 0}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ "hex_metadata.config\nmix.exs"
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: rejects a tarball that doesn't match the registry checksum before unpacking it" do
    in_diff_fixture(fn ->
      Hex.State.put(:diff_command, @git_diff_command)
      File.write!(Hex.SCM.cache_path("hexpm", "ex_doc", "0.1.0"), "not a tarball")
      Hex.State.put(:offline, true)

      # Unpacking first would fail with "Unpacking tarball failed" instead
      message =
        ~r/Checksum mismatch against registry \(outer\)|doesn't match the registry checksum/

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..0.1.0"])
      end

      refute_received {:mix_shell, :run, _}
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: package not found" do
    Hex.State.put(:shell_process, self())

    in_diff_fixture(fn ->
      assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
        Mix.Tasks.Hex.Package.run(["diff", "bad", "1.0.0..1.1.0"])
      end

      assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
        Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..2.0.0"])
      end

      assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
        Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "2.0.0"])
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  describe "search" do
    test "no results" do
      Mix.Tasks.Hex.Package.run(["search", "bloopdoopbloop"])
      assert_received {:mix_shell, :info, ["No packages found"]}
    end

    test "public packages" do
      Mix.Tasks.Hex.Package.run(["search", "doc"])
      assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
      assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
      assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
      assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
    end

    test "all private packages" do
      in_tmp(fn ->
        set_home_tmp()
        auth = Hexpm.new_user("searchuser1", "searchuser1@mail.com", "password", "searchuser1")
        Hexpm.new_repo("searchrepo1", auth)
        Hex.State.put(:api_key, auth[:key])

        Mix.Tasks.Hex.Package.run(["search", "doc"])

        assert_received {:mix_shell, :info, ["ex_doc" <> ex_doc]}
        assert_received {:mix_shell, :info, ["only_doc" <> only_doc]}
        assert ex_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/ex_doc"
        assert only_doc =~ ~r"\w*0\.1\.0.*http://localhost:4043/packages/only_doc"
      end)
    end

    test "org packages" do
      in_tmp(fn ->
        set_home_tmp()
        auth = Hexpm.new_user("searchuser2", "searchuser2@mail.com", "password", "searchuser2")
        Hexpm.new_repo("searchrepo2", auth)
        Hex.State.put(:api_key, auth[:key])

        Mix.Tasks.Hex.Package.run(["search", "doc", "--organization", "searchrepo2"])

        refute_received {:mix_shell, :info, ["ex_doc" <> _]}
        refute_received {:mix_shell, :info, ["only_doc" <> _]}
      end)
    end
  end
end
