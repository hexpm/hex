defmodule Mix.Tasks.Hex.ReleaseTest do
  use HexTest.Case
  @moduletag :integration

  defmodule ReleaseA.Mixfile do
    def project do
      [ app: :releasea, version: "0.0.1" ]
    end
  end

  defmodule ReleaseB.Mixfile do
    def project do
      [ app: :releaseb, version: "0.0.2",
        deps: [{ :ex_doc, "0.0.1", package: true }] ]
    end
  end

  @opts ["-u", "user", "-p", "hunter42"]

  setup do
    Hex.Registry.start(registry_path: tmp_path("hex.ets"))
  end

  test "validate" do
    assert_raise Mix.Error, "Missing command line option: pass", fn ->
      Mix.Tasks.Hex.Release.run(["--user", "release_name"])
    end
  end

  test "create and revert" do
    Mix.Project.push ReleaseA.Mixfile

    in_tmp fn _ ->
      File.mkdir_p("tmp")

      git_commit()
      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Release.run(@opts)
      assert_received { :mix_shell, :info, ["Successfully pushed releasea v0.0.1!"] }

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Release.run(@opts ++ ["--revert", "0.0.1"])
      assert_received { :mix_shell, :info, ["Successfully reverted releasea v0.0.1"] }
    end
  end

  test "create with deps" do
    Mix.Project.push ReleaseB.Mixfile

    in_tmp fn _ ->
      File.mkdir_p("tmp")

      git_commit()
      Mix.Tasks.Deps.Get.run([])

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Release.run(@opts)
      assert_received { :mix_shell, :info, ["Successfully pushed releaseb v0.0.2!"] }
    end
  after
    purge [Ex_doc.NoConflict.Mixfile]
  end

  defp git_commit do
    System.cmd("git init")
    System.cmd("git add .")
    System.cmd("git config user.email \"hex@example.com\"")
    System.cmd("git config user.name \"Hex Repo\"")
    System.cmd("git commit --allow-empty -m \"ok\"")
  end
end
