defmodule Mix.Tasks.Hex.ReleaseTest do
  use HexTest.Case
  @moduletag :integration

  defmodule ReleaseA.Mixfile do
    def project do
      [ app: :releasea, version: "0.0.1", source_url: "url" ]
    end
  end

  defmodule ReleaseB.Mixfile do
    def project do
      [ app: :releaseb, version: "0.0.2", source_url: "url",
       deps: [{ :ex_doc, "0.0.1", package: true }] ]
    end
  end

  @auth ["-u", "user", "-p", "hunter42"]

  setup do
    Hex.Registry.start [
      registry_path: tmp_path("hex.dets"),
      ram_file: true ]
  end

  test "validate" do
    assert_raise Mix.Error, "Missing command line option: password", fn ->
      Mix.Tasks.Hex.Release.run(["--user", "release_name"])
    end
  end

  test "create" do
    Mix.Project.push ReleaseA.Mixfile

    in_tmp fn _ ->
      File.mkdir_p("tmp")

      System.cmd("git init && git commit --allow-empty --allow-empty-message -m \"\"")
      Mix.Tasks.Hex.Release.run(@auth)
      assert_received { :mix_shell, :info, ["Updating package releasea and creating release 0.0.1 was successful!"] }

      Mix.Tasks.Hex.Release.run(@auth)
      assert_received { :mix_shell, :error, ["Creating release releasea 0.0.1 failed! (422)"] }
    end
  end

  test "create with deps" do
    Mix.Project.push ReleaseB.Mixfile

    in_tmp fn _ ->
      File.mkdir_p("tmp")

      System.cmd("git init && git commit --allow-empty --allow-empty-message -m \"\"")
      Mix.Tasks.Deps.Get.run([])

      Mix.Tasks.Hex.Release.run(@auth)
      assert_received { :mix_shell, :info, ["Updating package releaseb and creating release 0.0.2 was successful!"] }
    end
  after
    purge [Ex_doc.NoConflict.Mixfile]
  end
end
