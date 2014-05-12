defmodule Mix.Tasks.Hex.PublishTest do
  use HexTest.Case
  @moduletag :integration

  defmodule ReleaseSimple.Mixfile do
    def project do
      [ app: :releasea, version: "0.0.1" ]
    end
  end

  defmodule ReleaseDeps.Mixfile do
    def project do
      [ app: :releaseb, version: "0.0.2",
        deps: [{ :ex_doc, "0.0.1", package: true }] ]
    end
  end

  defmodule ReleaseMeta.Mixfile do
    def project do
      [ app: :releasec, version: "0.0.3",
        description: "foo",
        package: [files: ["myfile.txt"],
                  license: ["Apache"],
                  links: %{"a" => "b"},
                  contributors: ["contributors"]] ]
    end
  end 

  @opts ["-u", "user", "-p", "hunter42"]

  setup do
    Hex.Registry.start(registry_path: tmp_path("hex.ets"))
    :ok
  end

  test "validate" do
    assert_raise Mix.Error, "--pass option required if --user was given", fn ->
      Mix.Tasks.Hex.Publish.run(["--user", "release_name"])
    end
  end

  test "create and revert" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts)
      assert HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts ++ ["--revert", "0.0.1"])
      refute HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")
    end
  end

  test "create with key" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      System.put_env("MIX_HOME", tmp_path())

      user = HexWeb.User.get("user")
      { :ok, key } = HexWeb.API.Key.create("computer", user)
      Hex.Mix.update_config(username: "user", key: key.secret)

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run([])
      assert HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")
    end
  end

  test "create with deps" do
    Mix.Project.push ReleaseDeps.Mixfile

    in_tmp fn ->
      System.put_env("MIX_HOME", tmp_path())

      Mix.Tasks.Deps.Get.run([])

      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts)

      assert_received { :mix_shell, :info, ["  WARNING! No included files"] }
      assert_received { :mix_shell, :info, ["  WARNING! Missing metadata fields: description, licenses, contributors, links"] }
      assert HexWeb.Release.get(HexWeb.Package.get("releaseb"), "0.0.2")
    end
  after
    purge [Ex_doc.NoConflict.Mixfile]
  end

  test "create with meta" do
    Mix.Project.push ReleaseMeta.Mixfile

    in_tmp fn ->
      System.put_env("MIX_HOME", tmp_path())
      
      File.write!("myfile.txt", "hello")
      send self, { :mix_shell_input, :yes?, true }
      Mix.Tasks.Hex.Publish.run(@opts)

      assert_received { :mix_shell, :info, ["Publishing releasec v0.0.3"] }
      assert_received { :mix_shell, :info, ["  Included files:"] }
      assert_received { :mix_shell, :info, ["    myfile.txt"] }
      refute_received { :mix_shell, :info, ["  WARNING! Missing metadata fields" <> _] }
    end
  end
end
