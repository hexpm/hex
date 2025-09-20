defmodule Mix.Tasks.Hex.InfoTest do
  use HexTest.IntegrationCase

  defmodule Simple do
    def project do
      [
        app: :simple,
        version: "0.1.0",
        deps: [
          {:ecto, "0.2.0"}
        ]
      ]
    end
  end

  test "package" do
    Mix.Tasks.Hex.Info.run(["ex_doc"])
    assert_received {:mix_shell, :info, ["Some description\n"]}
    assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.1.0\"}"]}
    assert_received {:mix_shell, :info, ["Releases: 0.1.0, 0.1.0-rc1, 0.0.1\n"]}

    assert catch_throw(Mix.Tasks.Hex.Info.run(["no_package"])) == {:exit_code, 1}
    assert_received {:mix_shell, :error, ["No package with name no_package"]}

    assert catch_throw(Mix.Tasks.Hex.Info.run([""])) == {:exit_code, 1}
    assert_received {:mix_shell, :error, ["Package name is empty"]}
  end

  test "locked package" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      set_home_cwd()
      Mix.Task.run("deps.get")
      Mix.Task.clear()

      Mix.Tasks.Hex.Info.run(["ecto"])
      assert_received {:mix_shell, :info, ["Some description\n"]}
      assert_received {:mix_shell, :info, ["Locked version: 0.2.0"]}
      assert_received {:mix_shell, :info, ["Config: {:ecto, \"~> 3.3\"}"]}
      assert_received {:mix_shell, :info, ["Releases: 3.3.2, 3.3.1, 0.2.1, 0.2.0\n"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  test "package with retired release" do
    Mix.Tasks.Hex.Info.run(["tired"])
    assert_received {:mix_shell, :info, ["Releases: 0.2.0, 0.1.0 (retired)\n"]}
  end

  test "package with --organization flag" do
    in_tmp(fn ->
      set_home_cwd()
      Hex.State.put(:cache_home, tmp_path())

      # Set up authentication with API key
      auth = Hexpm.new_user("info_user", "info_user@mail.com", "hunter42", "key")
      Mix.Tasks.Hex.update_keys(auth[:key], auth[:key])

      # Add shell inputs for potential authentication prompts
      send(self(), {:mix_shell_input, :yes?, false})

      # Use an existing package that should be available
      Mix.Tasks.Hex.Info.run(["ex_doc", "--organization", "hexpm"])

      assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.1.0\"}"]}
    end)
  end

  test "release" do
    Mix.Tasks.Hex.Info.run(["ex_doc", "0.0.1"])
    assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.0.1\"}"]}

    Mix.Tasks.Hex.Info.run(["ex_doc", "0.1.0-rc1"])
    assert_received {:mix_shell, :info, ["Config: {:ex_doc, \"~> 0.1.0-rc1\"}"]}

    assert catch_throw(Mix.Tasks.Hex.Info.run(["ex_doc", "1.2.3"])) == {:exit_code, 1}
    assert_received {:mix_shell, :error, ["No release with name ex_doc 1.2.3"]}
  end

  test "prints publisher info for releases" do
    Mix.Tasks.Hex.Info.run(["ex_doc", "0.0.1"])
    assert_received {:mix_shell, :info, ["Published by: user (user@mail.com)"]}
  end
end
