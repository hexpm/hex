defmodule Mix.Tasks.Hex.OutdatedTest do
  use HexTest.Case
  @moduletag :integration

  defmodule OutdatedDeps.Mixfile do
    def project do
      [app: :outdated_app,
       version: "0.0.2",
       deps: [{:bar, "0.1.0"},
              {:ex_doc, "~> 0.0.1"}]]
    end
  end

  test "outdated" do
    Mix.Project.push OutdatedDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      Mix.Dep.Lock.write %{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}}

      Mix.Task.run "deps.get"
      flush

      Mix.Task.run "hex.outdated"

      bar = [:bright, "bar", :reset, "         ", "0.1.0", "    ", :green, "0.1.0", :reset, "   ", :green, "0.1.0", :reset]
            |> IO.ANSI.format
            |> List.to_string

      assert_received {:mix_shell, :info, [^bar]}
      refute_received {:mix_shell, :info, ["\e[1mfoo" <> _]}
    end
  end

  test "outdated --all" do
    Mix.Project.push OutdatedDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      Mix.Dep.Lock.write %{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}}

      Mix.Task.run "deps.get"
      flush

      Mix.Task.run "hex.outdated", ["--all"]

      bar = [:bright, "bar", :reset, "         ", "0.1.0", "    ", :green, "0.1.0", :reset, "   ", :green, "0.1.0", :reset]
            |> IO.ANSI.format
            |> List.to_string

      foo = [:bright, "foo", :reset, "         ", "0.1.0", "    ", :red, "0.1.1", :reset, "   ", :green, "~> 0.1.0", :reset]
            |> IO.ANSI.format
            |> List.to_string

      ex_doc = [:bright, "ex_doc", :reset, "      ", "0.0.1", "    ", :red, "0.1.0", :reset, "   ", :red, "~> 0.0.1", :reset]
               |> IO.ANSI.format
               |> List.to_string

      assert_received {:mix_shell, :info, [^bar]}
      assert_received {:mix_shell, :info, [^foo]}
      assert_received {:mix_shell, :info, [^ex_doc]}
    end
  end
end
