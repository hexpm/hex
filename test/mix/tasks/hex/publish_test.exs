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
        deps: [{:ex_doc, "0.0.1", package: true}] ]
    end
  end

  defmodule ReleaseMeta.Mixfile do
    def project do
      [ app: :releasec, version: "0.0.3",
        description: "foo",
        package: [files: ["myfile.txt", "missing.txt", "missing/*"],
                  licenses: ["Apache"],
                  links: %{"a" => "b"},
                  maintainers: ["maintainers"]] ]
    end
  end

  defmodule ReleaseName.Mixfile do
    def project do
      [ app: :released, version: "0.0.1",
        package: [name: :released_name] ]
    end
  end

  setup do
    Hex.Registry.open!(registry_path: tmp_path("registry.ets"))
    :ok
  end

  test "validate" do
    Mix.Project.push ReleaseSimple.Mixfile
    Hex.State.put(:home, "does_not_exist")

    assert_raise Mix.Error, "No authorized user found. Run 'mix hex.user auth'", fn ->
      Mix.Tasks.Hex.Publish.run([])
    end
  end

  test "package create and revert" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user")

      raised_message = """
        Invalid arguments, expected one of:
          mix hex.publish package
        """
      assert_raise Mix.Error, raised_message, fn ->
        Mix.Tasks.Hex.Publish.run(["--no-progress"])
      end

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Hex.Publish.run(["package","--no-progress"])
      assert HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")

      msg = "Before publishing, please read Hex Code of Conduct: https://hex.pm/docs/codeofconduct"
      assert_received {:mix_shell, :info, [^msg]}

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Hex.Publish.run(["--revert", "0.0.1"])
      refute HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")
    end
  end

  test "create with package name" do
    Mix.Project.push ReleaseName.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user")

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      release = HexWeb.Release.get(HexWeb.Package.get("released_name"), "0.0.1")
      assert release.meta["app"] == "released"
    end
  end

  test "create with key" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())

      user = HexWeb.User.get(username: "user")
      {:ok, key} = HexWeb.API.Key.create(user, %{"name" => "computer"})
      Hex.Config.update(username: "user", key: key.user_secret)

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      assert HexWeb.Release.get(HexWeb.Package.get("releasea"), "0.0.1")
    end
  end

  test "create with deps" do
    Mix.Project.push ReleaseDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user")

      Mix.Tasks.Deps.Get.run([])

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

      assert_received {:mix_shell, :info, ["\e[33m  WARNING! No files\e[0m"]}
      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Missing metadata fields: description, licenses, maintainers, links\e[0m"]}
      assert HexWeb.Release.get(HexWeb.Package.get("releaseb"), "0.0.2")
    end
  after
    purge [Ex_doc.NoConflict.Mixfile]
  end

  test "create with meta" do
    Mix.Project.push ReleaseMeta.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user")

      File.write!("myfile.txt", "hello")
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

      assert_received {:mix_shell, :info, ["Publishing releasec v0.0.3"]}
      assert_received {:mix_shell, :info, ["  Files:"]}
      assert_received {:mix_shell, :info, ["    myfile.txt"]}
      assert_received {:mix_shell, :info, ["\e[33m  WARNING! Missing files: missing.txt, missing/*" <> _]}
      refute_received {:mix_shell, :info, ["\e[33m  WARNING! Missing metadata fields" <> _]}
    end
  end
end
