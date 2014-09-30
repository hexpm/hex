defmodule Mix.Tasks.Docs do
  def run(_) do
    File.mkdir_p!("docs")
    File.write!("docs/index.html", "the index")
  end
end

defmodule Mix.Tasks.Hex.DocsTest do
  use HexTest.Case
  @moduletag :integration

  defmodule DocsSimple.Mixfile do
    def project do
      [app: :ex_doc, version: "0.0.1"]
    end
  end

  test "create and revert" do
    Mix.Project.push DocsSimple.Mixfile

    in_tmp fn ->
      Hex.home(tmp_path())
      setup_auth("user")

      Mix.Tasks.Hex.Docs.run(["--no-progress"])
      assert_received {:mix_shell, :info, ["Published docs for ex_doc v0.0.1"]}

      Mix.Tasks.Hex.Docs.run(["--revert", "0.0.1"])
      assert_received {:mix_shell, :info, ["Reverted docs for ex_doc v0.0.1"]}
    end
  end
end
