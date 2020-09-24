defmodule Mix.Tasks.Hex.PackageTest do
  use HexTest.Case
  @moduletag :integration

  defp in_diff_fixture(fun) do
    in_fixture("diff", fn ->
      Mix.Project.push(ReleaseDeps.MixProject)
      Mix.Dep.Lock.write(%{ex_doc: {:hex, :ex_doc, "0.0.1"}})
      Hex.State.put(:home, File.cwd!())
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
      Hex.State.put(:diff_command, "git diff --no-index --no-color __PATH1__ __PATH2__")

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
      Hex.State.put(:diff_command, "git diff --no-index --no-color __PATH1__ __PATH2__")

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
    in_tmp(fn ->
      Hex.State.put(:diff_command, "git diff --no-index --no-color __PATH1__ __PATH2__")

      assert catch_throw(Mix.Tasks.Hex.Package.run(["diff", "ex_doc", "0.0.1", "0.1.0"])) ==
               {:exit_code, 1}

      assert_received {:mix_shell, :run, [out]}
      assert out =~ ~s(-{<<"version">>,<<"0.0.1">>}.)
      assert out =~ ~s(+{<<"version">>,<<"0.1.0">>}.)
    end)
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
end
