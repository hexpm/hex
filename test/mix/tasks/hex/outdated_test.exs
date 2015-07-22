defmodule Mix.Tasks.Hex.OutdatedTest do
  use HexTest.Case
  @moduletag :integration

  defmodule OutdatedDeps.Mixfile do
    def project do
      [ app: :outdated_app,
        version: "0.0.2",
        deps: [ {:bar, "0.0.1"},
                {:eric, "0.2.0"} ]]
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

      Mix.Task.run "hex.outdated"

      assert_received {:mix_shell, :info, ["Outdated packages:"]}
      assert_received {:mix_shell, :info, [" * bar (0.2.0 > 0.1.0)"]}
      refute_received {:mix_shell, :info, [" * foo (0.2.1 > 0.1.0)"]}
    end
  end

  test "non outdated" do
    Mix.Project.push OutdatedDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      Mix.Dep.Lock.write %{eric: "0.2.0"}

      Mix.Task.run "hex.outdated"

      assert_received {:mix_shell, :info, ["All packages are up-to-date."]}
    end
  end

  test "outdated --all" do
    Mix.Project.push OutdatedDeps.Mixfile

    in_tmp fn ->
      Hex.State.put(:home, tmp_path())
      Mix.Dep.Lock.write %{bar: {:hex, :bar, "0.1.0"}, foo: {:hex, :foo, "0.1.0"}}

      Mix.Task.run "hex.outdated", ["--all"]

      assert_received {:mix_shell, :info, ["Outdated packages:"]}
      assert_received {:mix_shell, :info, [" * bar (0.2.0 > 0.1.0)"]}
      assert_received {:mix_shell, :info, [" * foo (0.2.1 > 0.1.0)"]}
    end
  end
end
