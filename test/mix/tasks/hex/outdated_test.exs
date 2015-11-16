defmodule Mix.Tasks.Hex.OutdatedTest do
  use HexTest.Case
  @moduletag :integration

  defmodule OutdatedDeps.Mixfile do
    def project do
      [app: :outdated_app,
       version: "0.0.2",
       deps: [{:bar, "0.1.0"},
              {:ex_doc, "0.1.0"}]]
    end
  end

  setup do
    Hex.Registry.open!(registry_path: tmp_path("registry.ets"))
    :ok
  end

  test "outdated" do
    Mix.Project.push OutdatedDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      Mix.Dep.Lock.write %{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}}

      Mix.Task.run "deps.get"
      flush

      Mix.Task.run "hex.outdated"

      assert_received {:mix_shell, :info, ["\e[1mbar\e[0m         \e[31m0.1.0\e[0m   0.2.0    \e[31m0.1.0\e[0m\e[0m"]}
      refute_received {:mix_shell, :info, ["\e[1mfoo\e[0m         \e[31m0.2.1\e[0m   0.2.0    \e[31m~> 0.1.0\e[0m\e[0m"]}
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

      assert_received {:mix_shell, :info, ["\e[1mbar\e[0m         \e[31m0.1.0\e[0m   0.2.0    \e[31m0.1.0\e[0m\e[0m"]}
      assert_received {:mix_shell, :info, ["\e[1mfoo\e[0m         \e[31m0.1.0\e[0m   0.2.1    \e[31m~> 0.1.0\e[0m\e[0m"]}
    end
  end
end
