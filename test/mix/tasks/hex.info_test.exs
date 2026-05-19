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
    assert_received {:mix_shell, :info, ["\nRecent releases:\n" <> releases]}
    today = Date.utc_today()
    assert releases == "  0.1.0 (#{today})\n  0.1.0-rc1 (#{today})\n  0.0.1 (#{today})\n"

    assert catch_throw(Mix.Tasks.Hex.Info.run(["no_package"])) == {:exit_code, 1}
    assert_received {:mix_shell, :error, ["No package with name no_package"]}

    assert catch_throw(Mix.Tasks.Hex.Info.run([""])) == {:exit_code, 1}
    assert_received {:mix_shell, :error, ["Package name is empty"]}
  end

  test "package downloads" do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    today = Date.utc_today()
    inserted_at = "#{today}T12:00:00Z"

    package_body = %{
      "name" => "ex_doc",
      "meta" => %{
        "description" => "Some description",
        "licenses" => ["GPL-2.0", "MIT", "Apache-2.0"],
        "links" => %{"docs" => "http://docs", "repo" => "http://repo"}
      },
      "configs" => %{"mix.exs" => "{:ex_doc, \"~> 0.1.0\"}"},
      "releases" => [
        %{"version" => "0.1.0", "inserted_at" => inserted_at},
        %{"version" => "0.1.0-rc1", "inserted_at" => inserted_at},
        %{"version" => "0.0.1", "inserted_at" => inserted_at}
      ],
      "retirements" => %{},
      "downloads" => %{
        "all" => 96_128_698,
        "day" => 21_494,
        "recent" => 1_421_136,
        "week" => 124_095
      }
    }

    Bypass.expect(bypass, "GET", "/api/packages/ex_doc", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
      |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(package_body))
    end)

    Mix.Tasks.Hex.Info.run(["ex_doc"])
    assert_received {:mix_shell, :info, ["Downloads:\n" <> downloads]}

    assert String.split(downloads, "\n") == [
             "  Yesterday: 21 494",
             "  Last 7 days: 124 095",
             "  All time: 96 128 698",
             ""
           ]
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
      assert_received {:mix_shell, :info, ["\nRecent releases:\n" <> releases]}

      today = Date.utc_today()

      assert String.split(releases, "\n") == [
               "  3.3.2 (#{today})",
               "  3.3.1 (#{today})",
               "  0.2.1 (#{today})",
               "  0.2.0 (#{today})",
               ""
             ]
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
    today = Date.utc_today()
    assert_received {:mix_shell, :info, ["\nRecent releases:\n" <> releases]}
    assert releases == "  0.2.0 (#{today})\n  0.1.0 (#{today}) (retired)\n"
  end

  test "package with --organization flag" do
    in_tmp(fn ->
      set_home_cwd()
      Hex.State.put(:cache_home, tmp_path())

      # Set up authentication with API key
      auth = Hexpm.new_user("info_user", "info_user@mail.com", "hunter42", "key")
      Hex.State.put(:api_key, auth[:key])

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

  test "release downloads" do
    bypass = Bypass.open()
    Hex.State.put(:api_url, "http://localhost:#{bypass.port}/api")

    today = Date.utc_today()
    inserted_at = "#{today}T12:00:00Z"

    release_body = %{
      "version" => "1.5.0-alpha.2",
      "checksum" => "6dcaa0d9fdc22afe9b4d362f17f20844a85f121c50b6e9b9466ac04fe39f3665",
      "inserted_at" => inserted_at,
      "updated_at" => inserted_at,
      "retirement" => nil,
      "publisher" => nil,
      "downloads" => 26_208,
      "configs" => %{
        "erlang.mk" => "dep_jason = hex 1.5.0-alpha.2",
        "mix.exs" => "{:jason, \"~\u003E 1.5.0-alpha.2\"}",
        "rebar.config" => "{jason, \"1.5.0-alpha.2\"}"
      }
    }

    Bypass.expect(bypass, "GET", "/api/packages/ex_doc/releases/0.1.0", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("content-type", "application/vnd.hex+erlang")
      |> Plug.Conn.resp(200, Hex.Utils.safe_serialize_erlang(release_body))
    end)

    Mix.Tasks.Hex.Info.run(["ex_doc", "0.1.0"])
    assert_received {:mix_shell, :info, ["Downloads: 26 208"]}
  end

  test "prints publisher info for releases" do
    Mix.Tasks.Hex.Info.run(["ex_doc", "0.0.1"])
    assert_received {:mix_shell, :info, ["Published by: user (user@mail.com)"]}
  end

  test "prints release date for releases" do
    Mix.Tasks.Hex.Info.run(["ex_doc", "0.0.1"])
    assert_received {:mix_shell, :info, ["Released: " <> date]}
    assert date == "#{Date.utc_today()}"
  end
end
