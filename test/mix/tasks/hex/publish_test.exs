defmodule Mix.Tasks.Hex.PublishTest do
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
    assert_raise Mix.Error, "--pass option required if --user was given", fn ->
      Mix.Tasks.Hex.Publish.run(["--user", "release_name"])
    end
  end

  test "create and revert" do
    Mix.Project.push ReleaseA.Mixfile

    in_tmp fn _ ->
      File.mkdir_p("tmp")

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts)
      assert HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts ++ ["--revert", "0.0.1"])
      refute HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")
    end
  end

  test "create with key" do
    Mix.Project.push ReleaseA.Mixfile

    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)

      user = HexWeb.User.get("user")
      { :ok, key } = HexWeb.API.Key.create("computer", user)
      Hex.Mix.update_config(username: "user", key: key.secret)

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run([])
      assert HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")
    end
  after
    System.delete_env("MIX_HOME")
  end

  test "create with deps" do
    Mix.Project.push ReleaseB.Mixfile

    in_tmp fn _ ->
      System.put_env("MIX_HOME", System.cwd!)
      File.mkdir_p("tmp")

      Mix.Tasks.Deps.Get.run([])

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts)
      assert HexWeb.Release.get(HexWeb.Package.get("releaseb"), "0.0.2")
    end
  after
    purge [Ex_doc.NoConflict.Mixfile]
    System.delete_env("MIX_HOME")
  end
end
