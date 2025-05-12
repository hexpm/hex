defmodule Mix.Tasks.Hex.OutdatedTest do
  use HexTest.IntegrationCase

  defmodule OutdatedDeps.MixProject do
    def project do
      [
        app: :outdated_app,
        version: "0.0.2",
        deps: [
          {:bar, "0.1.0"},
          {:ex_doc, "~> 0.0.1"}
        ]
      ]
    end
  end

  defmodule OutdatedBetaDeps.MixProject do
    def project do
      [
        app: :outdated_app,
        version: "0.0.1",
        deps: [
          {:beta, ">= 0.0.0"}
        ]
      ]
    end
  end

  defmodule OutdatedApp.MixProject do
    def project do
      [
        app: :outdated_app,
        version: "0.0.1",
        deps: [
          {:ex_doc, ">= 0.0.0"},
          {:postgrex, "0.2.0"},
          {:ecto, "0.2.0"}
        ]
      ]
    end
  end

  defmodule NotOutdatedApp.MixProject do
    def project do
      [
        app: :outdated_app,
        version: "0.0.1",
        deps: [
          {:ex_doc, ">= 0.0.0"}
        ]
      ]
    end
  end

  defmodule WithoutHexDeps.MixProject do
    def project do
      [
        app: :outdated_app,
        version: "0.0.1",
        deps: []
      ]
    end
  end

  defmodule OutdatedMultiDeps.MixProject do
    def project do
      [
        app: :outdated_app,
        version: "0.0.2",
        deps: [
          {:baz, "0.1.0"},
          {:bar, "0.1.0"}
        ]
      ]
    end
  end

  test "outdated" do
    Mix.Project.push(OutdatedDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}})

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated")) == {:exit_code, 1}

      bar =
        [
          [:bright, "bar", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :green, "0.1.0", :reset],
          ["   ", :green, "Up-to-date", :reset],
          "           "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^bar]}
      refute_received {:mix_shell, :info, ["foo" <> _]}
    end)
  end

  test "outdated --all" do
    Mix.Project.push(OutdatedDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}})

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated", ["--all"])) == {:exit_code, 1}

      bar =
        [
          [:bright, "bar", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :green, "0.1.0", :reset],
          ["   ", :green, "Up-to-date", :reset],
          "           "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      foo =
        [
          [:bright, "foo", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :red, "0.1.1", :reset],
          ["   ", :yellow, "Update possible", :reset],
          "      "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      ex_doc =
        [
          [:bright, "ex_doc", :reset],
          ["      ", "0.0.1", :reset],
          ["    ", :red, "0.1.0", :reset],
          ["   ", :red, "Update not possible", :reset],
          "  "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^bar]}
      assert_received {:mix_shell, :info, [^foo]}
      assert_received {:mix_shell, :info, [^ex_doc]}
    end)
  end

  test "outdated --all --sort status" do
    Mix.Project.push(OutdatedApp.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}})

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated", ["--all", "--sort", "status"])) ==
               {:exit_code, 1}

      _bar =
        [
          [:bright, "bar", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :green, "0.1.0", :reset],
          ["   ", :green, "Up-to-date", :reset],
          "           "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      _foo =
        [
          [:bright, "foo", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :red, "0.1.1", :reset],
          ["   ", :yellow, "Update possible", :reset],
          "      "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      _ex_doc =
        [
          [:bright, "ex_doc", :reset],
          ["      ", "0.0.1", :reset],
          ["    ", :red, "0.1.0", :reset],
          ["   ", :red, "Update not possible", :reset],
          "  "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      lines = flush()
      extracted_statuses = extract_statuses(lines)

      assert extracted_statuses == [
               "Up-to-date",
               "Update not possible",
               "Update not possible",
               "Update not possible",
               "Update possible"
             ]
    end)
  end

  test "outdated --all with multiple dependent packages" do
    Mix.Project.push(OutdatedMultiDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()

      Mix.Dep.Lock.write(%{
        foo: {:hex, :foo, "0.1.0"},
        bar: {:hex, :bar, "0.1.0"},
        baz: {:hex, :baz, "0.1.0"}
      })

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated", ["--all"])) == {:exit_code, 1}

      foo =
        [
          [:bright, "foo", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :red, "0.1.1", :reset],
          ["   ", :red, "Update not possible", :reset],
          "  "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^foo]}
    end)
  end

  test "outdated --all --within-requirements" do
    Mix.Project.push(OutdatedDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}})

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated", ["--all", "--within-requirements"])) ==
               {:exit_code, 1}

      bar =
        [
          [:bright, "bar", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :green, "0.1.0", :reset],
          ["   ", :green, "Up-to-date", :reset],
          "           "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      foo =
        [
          [:bright, "foo", :reset],
          ["         ", "0.1.0", :reset],
          ["    ", :red, "0.1.1", :reset],
          ["   ", :yellow, "Update possible", :reset],
          "      "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      ex_doc =
        [
          [:bright, "ex_doc", :reset],
          ["      ", "0.0.1", :reset],
          ["    ", :red, "0.1.0", :reset],
          ["   ", :red, "Update not possible", :reset],
          "  "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^bar]}
      assert_received {:mix_shell, :info, [^foo]}
      assert_received {:mix_shell, :info, [^ex_doc]}
    end)
  end

  test "outdated --all --within-requirements (not outdated)" do
    Mix.Project.push(NotOutdatedApp.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{ex_doc: {:hex, :ex_doc, "0.1.0"}})

      Mix.Task.run("deps.get")
      flush()

      assert Mix.Task.run("hex.outdated", ["ex_doc"]) == nil

      msg =
        ["Current version ", :bright, "0.1.0", :reset, " of dependency is up to date!"]
        |> IO.ANSI.format_fragment()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^msg]}
    end)
  end

  test "outdated --pre" do
    Mix.Project.push(OutdatedBetaDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{beta: {:hex, :beta, "1.0.0"}})

      Mix.Task.run("deps.get")
      flush()

      Mix.Task.run("hex.outdated", [])

      beta =
        [
          [:bright, "beta", :reset],
          ["        ", "1.0.0", :reset],
          ["    ", :green, "1.0.0", :reset],
          ["   ", :green, "Up-to-date", :reset],
          ["  "]
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^beta]}

      Mix.Task.reenable("hex.outdated")
      assert catch_throw(Mix.Task.run("hex.outdated", ["--pre"])) == {:exit_code, 1}

      beta =
        [
          [:bright, "beta", :reset],
          ["        ", "1.0.0", :reset],
          ["    ", :red, "1.1.0-beta", :reset],
          ["  ", :yellow, "Update possible", :reset],
          "  "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^beta]}
    end)
  end

  test "outdated app" do
    Mix.Project.push(OutdatedApp.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{ex_doc: {:hex, :ex_doc, "0.0.1"}})

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated", ["ex_doc"])) == {:exit_code, 1}

      msg =
        [
          "There is newer version of the dependency available ",
          [:bright, "0.1.0 > 0.0.1", :reset, "!"]
        ]
        |> IO.ANSI.format_fragment()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^msg]}

      mix =
        [
          :bright,
          "mix.exs",
          :reset,
          "   ",
          :green,
          ">= 0.0.0",
          :reset,
          "     ",
          :green,
          "Yes",
          :reset,
          "         "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^mix]}

      ecto =
        [
          :bright,
          "ecto",
          :reset,
          "      ",
          :red,
          "~> 0.0.1",
          :reset,
          "     ",
          :red,
          "No",
          :reset,
          "          "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^ecto]}

      postgrex =
        [
          :bright,
          "postgrex",
          :reset,
          "  ",
          :red,
          "0.0.1",
          :reset,
          "        ",
          :red,
          "No",
          :reset,
          "          "
        ]
        |> IO.ANSI.format()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^postgrex]}
    end)
  end

  test "outdated app --json" do
    Mix.Project.push(OutdatedApp.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{ex_doc: {:hex, :ex_doc, "0.0.1"}})

      Mix.Task.run("deps.get")
      flush()

      assert catch_throw(Mix.Task.run("hex.outdated", ["ex_doc", "--json"])) == {:exit_code, 1}

      msg = Jason.encode!([%{
        outdated: true,
        requirements: [
          %{
            source: "mix.exs",
            requirement: ">= 0.0.0",
            up_to_date: true
          },
          %{
            source: "ecto",
            requirement: "~> 0.0.1",
            up_to_date: false
          },
          %{
            source: "postgrex",
            requirement: "0.0.1",
            up_to_date: false
          }
        ],
        package: "ex_doc",
        lock_version: "0.0.1",
        latest_version: "0.1.0"
      }])

      assert_received {:mix_shell, :info, [^msg]}
    end)
  end

  test "not outdated app" do
    Mix.Project.push(NotOutdatedApp.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{ex_doc: {:hex, :ex_doc, "0.1.0"}})

      Mix.Task.run("deps.get")
      flush()

      Mix.Task.run("hex.outdated", ["ex_doc"])

      msg =
        ["Current version ", :bright, "0.1.0", :reset, " of dependency is up to date!"]
        |> IO.ANSI.format_fragment()
        |> List.to_string()

      assert_received {:mix_shell, :info, [^msg]}
    end)
  end

  test "without hex deps" do
    Mix.Project.push(WithoutHexDeps.MixProject)

    in_tmp(fn ->
      set_home_tmp()
      Mix.Dep.Lock.write(%{})

      Mix.Task.run("deps.get")
      flush()

      Mix.Task.run("hex.outdated")
      msg = "No hex dependencies"
      assert_received {:mix_shell, :info, [^msg]}
    end)
  end

  test "umbrella projects" do
    in_tmp("umbrella", fn ->
      File.write!("mix.exs", """
      defmodule Umbrella.MixProject do
        use Mix.Project

        def project do
          [apps_path: "apps",
           version: "0.0.1",
           deps: [{:ex_doc, "~> 0.0.1"}]]
        end
      end
      """)

      Mix.Project.in_project(:umbrella, ".", fn _ ->
        File.mkdir_p!("apps/bacon")

        File.write!("apps/bacon/mix.exs", """
        defmodule Bacon.MixProject do
          use Mix.Project

          def project do
            [app: :bacon,
             version: "0.1.0",
             build_path: "../../_build",
             config_path: "../../config/config.exs",
             deps_path: "../../deps",
             lockfile: "../../mix.lock",
             deps: [{:bar, "0.1.0"}]]
          end
        end
        """)

        Mix.Project.in_project(:bacon, "apps/bacon", fn _ ->
          Mix.Task.run("deps.get")
          flush()
        end)

        Mix.Task.run("deps.get")
        flush()

        ex_doc =
          [
            [:bright, "ex_doc", :reset],
            ["      ", "0.0.1", :reset],
            ["    ", :red, "0.1.0", :reset],
            ["   ", :red, "Update not possible", :reset],
            "  "
          ]
          |> IO.ANSI.format()
          |> List.to_string()

        bar =
          [
            [:bright, "bar", :reset],
            ["         ", "0.1.0", :reset],
            ["    ", :green, "0.1.0", :reset],
            ["   ", :green, "Up-to-date", :reset],
            "           "
          ]
          |> IO.ANSI.format()
          |> List.to_string()

        foo =
          [
            [:bright, "foo", :reset],
            ["         ", "0.1.1", :reset],
            ["    ", :green, "0.1.1", :reset],
            ["   ", :green, "Up-to-date", :reset],
            "           "
          ]
          |> IO.ANSI.format()
          |> List.to_string()

        assert catch_throw(Mix.Task.run("hex.outdated")) == {:exit_code, 1}
        assert_received {:mix_shell, :info, [^ex_doc]}
        assert_received {:mix_shell, :info, [^bar]}
        refute_received {:mix_shell, :info, [^foo]}

        assert catch_throw(Mix.Tasks.Hex.Outdated.run(["--all"])) == {:exit_code, 1}
        assert_received {:mix_shell, :info, [^ex_doc]}
        assert_received {:mix_shell, :info, [^bar]}
        assert_received {:mix_shell, :info, [^foo]}
      end)
    end)
  end

  defp extract_statuses(lines) do
    Enum.flat_map(lines, fn {_, _, [line]} ->
      ~r/Up-to-date|Update not possible|Update possible/
      |> Regex.scan(line)
      |> List.flatten()
    end)
  end
end
