defmodule Mix.Tasks.Docs do
  def run(_) do
    File.mkdir_p!("doc")
    File.write!("doc/index.html", "the index")
  end
end

defmodule Mix.Tasks.Hex.PublishTest do
  use HexTest.Case
  @moduletag :integration

  defmodule DocsSimple.Mixfile do
    def project do
      [app: :ex_doc, version: "0.1.0"]
    end
  end

  defmodule DocsError.Mixfile do
    def project do
      [app: :ex_doc, version: "0.1.1"]
    end
  end

  test "validate" do
    Mix.Project.push ReleaseSimple.Mixfile
    Hex.State.put(:home, tmp_path("does_not_exist"))

    assert_raise Mix.Error, "No authorized user found. Run 'mix hex.user auth'", fn ->
      Mix.Tasks.Hex.Publish.run([])
    end
  after
    purge [ReleaseSimple.Mixfile]
  end

  test "create and revert a package" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send self(), {:mix_shell_input, :yes?, true}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      assert {200, _, _} = Hex.API.Release.get("release_a", "0.0.1")

      msg = "Before publishing, please read the Code of Conduct: https://hex.pm/policies/codeofconduct"
      assert_received {:mix_shell, :info, [^msg]}

      send self(), {:mix_shell_input, :yes?, true}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["package", "--revert", "0.0.1"])
      assert {404, _, _} = Hex.API.Release.get("release_a", "0.0.1")
    end
  after
    purge [ReleaseSimple.Mixfile]
  end

  test "create and revert docs" do
    Mix.Project.push DocsSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress"])
      assert_received {:mix_shell, :info, ["Docs published to https://hexdocs.pm/ex_doc/0.1.0"]}

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["docs", "--revert", "0.1.0"])
      assert_received {:mix_shell, :info, ["Reverted docs for ex_doc 0.1.0"]}
    end
  end

  test "docs when package is not published yet" do
    Mix.Project.push DocsError.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["docs", "--no-progress"])
      assert_received {:mix_shell, :error, ["Publishing docs failed due to the package not being published yet"]}

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["--revert", "0.1.1"])
      assert_received {:mix_shell, :error, ["Reverting docs for ex_doc 0.1.1 failed"]}
    end
  end

  test "package create with package name" do
    Mix.Project.push ReleaseName.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      raised_message = """
      Invalid arguments, expected one of:
      mix hex.publish
      mix hex.publish package
      mix hex.publish docs
      """

      send self(), {:mix_shell_input, :prompt, "hunter42"}
      assert_raise Mix.Error, raised_message, fn ->
        Mix.Tasks.Hex.Publish.run(["invalid", "--no-progress"])
      end

      send self(), {:mix_shell_input, :yes?, true}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["--no-progress"])
      msg = "Publishing released_name 0.0.1"
      assert_received {:mix_shell, :info, [^msg]}

      send self(), {:mix_shell_input, :yes?, true}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      assert {200, body, _} = Hex.API.Release.get("released_name", "0.0.1")
      assert body["meta"]["app"] == "release_d"
    end
  after
    purge [ReleaseName.Mixfile]
  end

  test "create with key" do
    Mix.Project.push ReleaseSimple.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      send self(), {:mix_shell_input, :yes?, true}
      send self(), {:mix_shell_input, :prompt, "hunter42"}
      Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])
      assert {200, _, _} = Hex.API.Release.get("release_a", "0.0.1")
    end
  after
    purge [ReleaseSimple.Mixfile]
  end

  test "create with deps" do
    Mix.Project.push ReleaseDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      Mix.Tasks.Deps.Get.run([])

      error_msg = "Stopping package build due to errors.\n" <>
                  "Missing metadata fields: maintainers, links"

      assert_raise Mix.Error, error_msg, fn ->
        send self(), {:mix_shell_input, :yes?, true}
        send self(), {:mix_shell_input, :prompt, "hunter42"}
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

        assert_received {:mix_shell, :error, ["No files"]}
        assert {200, _, _} = Hex.API.Release.get("release_b", "0.0.2")
      end
    end
  after
    purge [ReleaseDeps.Mixfile]
  end

  test "create with meta" do
    Mix.Project.push ReleaseMeta.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      setup_auth("user", "hunter42")

      error_msg = "Stopping package build due to errors.\n" <>
                  "Missing files: missing.txt, missing/*"

      assert_raise Mix.Error, error_msg, fn ->
        File.write!("myfile.txt", "hello")
        send self(), {:mix_shell_input, :yes?, true}
        send self(), {:mix_shell_input, :prompt, "hunter42"}
        Mix.Tasks.Hex.Publish.run(["package", "--no-progress"])

        assert_received {:mix_shell, :info, ["Publishing release_c 0.0.3"]}
        assert_received {:mix_shell, :info, ["  Files:"]}
        assert_received {:mix_shell, :info, ["    myfile.txt"]}
        assert_received {:mix_shell, :info, ["  Extra: \n    c: d"]}
        refute_received {:mix_shell, :error, ["Missing metadata fields" <> _]}
      end
    end
  after
    purge [ReleaseMeta.Mixfile]
  end
end
