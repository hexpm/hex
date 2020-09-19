defmodule Mix.Tasks.Hex.PackageTest do
  use HexTest.Case
  @moduletag :integration

  test "fetch: success" do
    in_tmp(fn ->
      cwd = File.cwd!()
      Mix.Tasks.Hex.Package.run(["fetch", "ex_doc", "0.0.1"])
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

  #     Hex.unpack_tar!({:binary, tarball}, :memory)
  #   end)
  # end

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
    Mix.Project.push(ReleaseDeps.MixProject)
    {:ok, cwd} = File.cwd()

    in_tmp(fn ->
      path = tmp_path()
      File.cp(Path.join(cwd, "mix-test.lock"), Path.join(path, "mix.lock"))
      Hex.State.put(:diff_command, "git diff --no-index --no-color __PATH1__ __PATH2__")
      Hex.State.put(:home, path)
      Mix.Tasks.Deps.Get.run([])

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.1.0"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ ~s(-{<<"version">>,<<"0.0.1">>}.)
      assert out =~ ~s(+{<<"version">>,<<"0.1.0">>}.)
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: not Mix.Project with single version number" do
    msg =
      "Cannot execute \"mix diff ex_doc 1.0.0\" without a Mix.Project, " <>
        "please ensure you are running Mix in a directory with a \"mix.exs\" file"

    assert_raise Mix.Error, msg, fn ->
      Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "1.0.0"])
    end
  end

  test "diff: outdated lockfile with single version number" do
    msg =
      "The dependency is out of date. " <>
        "Please run \"deps.get\" to update \"mix.lock\" file"

    Mix.Project.push(ReleaseDeps.MixProject)
    {:ok, cwd} = File.cwd()

    in_tmp(fn ->
      assert_raise Mix.Error, msg, fn ->
        path = tmp_path()
        File.cp(Path.join(cwd, "mix-test.lock"), Path.join(path, "mix.lock"))
        Mix.Tasks.Deps.Get.run([])

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
      "Cannot find the package \"tesla\" in \"mix.lock\" file. " <>
        "Please ensure it has been specified in \"mix.exs\" and run \"mix deps.get\""

    Mix.Project.push(ReleaseDeps.MixProject)
    {:ok, cwd} = File.cwd()

    in_tmp(fn ->
      assert_raise Mix.Error, msg, fn ->
        path = tmp_path()
        File.cp(Path.join(cwd, "mix-test.lock"), Path.join(path, "mix.lock"))
        Mix.Tasks.Deps.Get.run([])

        Mix.Tasks.Hex.Package.run(["diff", "tesla", "1.0.0"])
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end

  test "diff: success with version range" do
    in_tmp(fn ->
      Hex.State.put(:diff_command, "git diff --no-index --no-color __PATH1__ __PATH2__")

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..0.1.0"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ ~s(-{<<"version">>,<<"0.0.1">>}.)
      assert out =~ ~s(+{<<"version">>,<<"0.1.0">>}.)
    end)
  end

  test "diff: custom diff command" do
    in_tmp(fn ->
      Hex.State.put(:diff_command, "ls __PATH1__ __PATH2__")

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..0.1.0"])) ==
               {:exit_code, 0}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ "hex_metadata.config\nmix.exs"
    end)
  end

  test "diff: package not found" do
    Hex.State.put(:shell_process, self())

    assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
      Mix.Tasks.Hex.Package.run(["diff", "bad", "1.0.0..1.1.0"])
    end

    in_tmp(fn ->
      assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
        Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1..2.0.0"])
      end
    end)

    Mix.Project.push(ReleaseDeps.MixProject)
    {:ok, cwd} = File.cwd()

    in_tmp(fn ->
      path = tmp_path()
      File.cp(Path.join(cwd, "mix-test.lock"), Path.join(path, "mix.lock"))
      Hex.State.put(:home, path)

      Mix.Tasks.Deps.Get.run([])

      assert_raise Mix.Error, ~r"Request failed \(404\)", fn ->
        Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "2.0.0"])
      end
    end)
  after
    purge([ReleaseDeps.MixProject])
  end
end
