defmodule Hex.MixTaskTest do
  use HexTest.Case
  @moduletag :integration

  defmodule Simple do
    def project do
      [app: :simple,
       version: "0.1.0",
       deps: [{:ecto, "0.2.0"}]]
    end
  end

  defmodule SimpleOld do
    def project do
      [app: :simple,
       version: "0.1.0",
       deps: [{:ecto, "~> 0.2.1"}]]
    end
  end

  defmodule EctoDep do
    def project do
      [app: :simple,
       version: "0.1.0",
       deps: [{:ecto, "~> 0.2.0"}]]
    end
  end

  defmodule Override do
    def project do
      [app: :override,
       version: "0.1.0",
       deps: [{:ecto, "0.2.0"},
              {:ex_doc, "~> 0.1.0", override: true}]]
    end
  end

  defmodule NonHexDep do
    def project do
      [app: :non_hex_dep,
       version: "0.1.0",
       deps: [{:has_hex_dep, path: fixture_path("has_hex_dep")}]]
    end
  end

  defmodule EctoPathDep do
    def project do
      [app: :ecto_path_dep,
       version: "0.1.0",
       deps: [{:postgrex, ">= 0.0.0"},
              {:ecto, path: fixture_path("ecto")}]]
    end
  end

  defmodule OverrideWithPath do
    def project do
      [app: :override_with_path,
       version: "0.1.0",
       deps: [{:postgrex, ">= 0.0.0"},
              {:ex_doc, path: fixture_path("ex_doc"), override: true}]]
    end
  end

  defmodule OverrideTwoLevelsWithPath do
    def project do
      [app: :override_two_levels_with_path,
       version: "0.1.0",
       deps: [{:phoenix, ">= 0.0.0"},
              {:ex_doc, path: fixture_path("ex_doc"), override: true}]]
    end
  end

  defmodule OverrideWithPathParent do
    def project do
      [app: :override_with_path_parent,
       version: "0.1.0",
       deps: [{:override_with_path, path: fixture_path("override_with_path")}]]
    end
  end

  defmodule Optional do
    def project do
      [app: :optional,
       version: "0.1.0",
       deps: [{:only_doc, ">= 0.0.0"}]]
    end
  end

  defmodule WithOptional do
    def project do
      [app: :with_optional,
       version: "0.1.0",
       deps: [{:only_doc, ">= 0.0.0"},
              {:ex_doc, "0.0.1"}]]
    end
  end

  defmodule WithPackageName do
    def project do
      [app: :with_package_name,
       version: "0.1.0",
       deps: [{:app_name, ">= 0.0.0", hex: :package_name}]]
    end
  end

  defmodule WithDependName do
    def project do
      [app: :with_depend_name,
       version: "0.1.0",
       deps: [{:depend_name, ">= 0.0.0"}]]
    end
  end

  defmodule WithIncorrectDepVersion do
    def project do
      [app: :with_incorrect_dep_version,
       version: "0.1.0",
       deps: [{:ex_doc, "> hello"}]]
    end
  end

  defmodule WithMissingDepVersion do
    def project do
      [app: :with_missing_dep_version,
       version: "0.1.0",
       deps: [{:ex_doc, []}]]
    end
  end

  defmodule WithNonMatchingRequirement do
    def project do
      [app: :with_non_matching_requirement,
       version: "0.1.0",
       deps: [{:ex_doc, "~> 100.0.0"}]]
    end
  end

  defmodule WithOnlyMatchingPreRequirement do
    def project do
      [app: :with_only_matching_pre_requirement,
       version: "0.1.0",
       deps: [{:beta, "~> 1.1.0"}]]
    end
  end

  test "deps.get" do
    Mix.Project.push Simple

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting ecto (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}

      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* ecto 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (ecto)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* postgrex 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (postgrex)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* ex_doc 0.0.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.0.1 (ex_doc)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end

  test "deps.get with lock" do
    Mix.Project.push Simple

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"
      Mix.Task.clear

      Mix.Task.run "deps.get"
      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* ecto 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* postgrex 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc 0.0.1 (Hex package)" <> _]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end

  test "deps.update" do
    Mix.Project.push Simple

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      # `deps.get` to set up lock
      Mix.Task.run "deps.get"

      purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
             Ex_doc.NoConflict.MixProject]

      Mix.ProjectStack.clear_cache
      Mix.Project.pop
      Mix.Project.push SimpleOld

      Mix.Task.run "deps.update", ["ecto"]

      assert_received {:mix_shell, :info, ["* Updating ecto (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Updating postgrex (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Updating ex_doc (Hex package)"]}

      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* ecto 0.2.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.1 (ecto)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* postgrex 0.2.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.1 (postgrex)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* ex_doc 0.1.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (ex_doc)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject, Sample.Fixture.MixProject]
  end

  test "deps.update locked dependency" do
    Mix.Project.push EctoDep

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      File.write!("mix.lock", ~s(%{"ecto": {:hex, :ecto, "0.2.0"}}))
      Mix.Task.run "deps.update", ["ecto"]

      assert_received {:mix_shell, :info, ["\e[32m  ecto 0.2.1\e[0m"]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject, Sample.Fixture.MixProject]
  end

  test "deps.get with override" do
    Mix.Project.push Override

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"
      Mix.Task.run "deps.compile"
      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* ecto 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (ecto)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* postgrex 0.2.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.1 (postgrex)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* ex_doc 0.1.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (ex_doc)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end

  test "deps.get with non hex dependency that has hex dependency" do
    Mix.Project.push NonHexDep

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting ecto (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject, HasHexDep.Fixture.MixProject]
  end

  test "converged hex dependency considers all requirements" do
    Mix.Project.push EctoPathDep

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}

      assert %{postgrex: {:hex, :postgrex, "0.2.0", _, _, _, "hexpm"}} = Mix.Dep.Lock.read
    end
  after
    purge [Ecto.Fixture.MixProject, Postgrex.NoConflict.MixProject, Ex_doc.NoConflict.MixProject]
  end

  test "do not fetch git children of hex dependencies" do
    Mix.Project.push SimpleOld

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting ecto (Hex package)"]}

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* ecto (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* sample" <> _]}
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end

  test "override hex dependency with path dependency" do
    Mix.Project.push OverrideWithPath

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* postgrex (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc" <> _]}

      assert %{postgrex: {:hex, :postgrex, "0.2.1", _, _, _, "hexpm"}} = Mix.Dep.Lock.read
    end
  after
    purge [Postgrex.NoConflict.MixProject, ExDoc.Fixture.MixProject]
  end

  test "override hex dependency two levels down with path dependency" do
    Mix.Project.push OverrideTwoLevelsWithPath

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* phoenix (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* postgrex (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc" <> _]}

      assert %{phoenix: {:hex, :phoenix, "0.0.1", _, _, _, "hexpm"},
               postgrex: {:hex, :postgrex, "0.2.1", _, _, _, "hexpm"}} = Mix.Dep.Lock.read
    end
  after
    purge [Phoenix.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           ExDoc.Fixture.MixProject]
  end

  test "override hex dependency with path dependency from dependency" do
    Mix.Project.push OverrideWithPathParent

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)
      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* postgrex (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc" <> _]}

      assert %{postgrex: {:hex, :postgrex, "0.2.1", _, _, _, "hexpm"}} = Mix.Dep.Lock.read
    end
  after
    purge [OverrideWithPath.Fixture.MixProject, ExDoc.Fixture.MixProject,
           Postgrex.NoConflict.MixProject]
  end

  test "optional dependency" do
    Mix.Project.push Optional

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting only_doc (Hex package)"]}
      refute_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* only_doc (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
    end
  after
    purge [Only_doc.NoConflict.MixProject, Ex_doc.NoConflict.MixProject]
  end

  test "with optional dependency" do
    Mix.Project.push WithOptional

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting only_doc (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* only_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.0.1 (ex_doc)" <> _]}
    end
  after
    purge [Only_doc.NoConflict.MixProject, Ex_doc.NoConflict.MixProject]
  end

  test "with package name" do
    Mix.Project.push WithPackageName

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting app_name (Hex package)"]}

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* app_name (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (package_name)" <> _]}
    end
  after
    purge [Package_name.NoConflict.MixProject]
  end

  test "with depend name" do
    Mix.Project.push WithDependName

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"

      assert_received {:mix_shell, :info, ["* Getting depend_name (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting app_name (Hex package)"]}

      Mix.Task.run "deps"

      assert_received {:mix_shell, :info, ["* depend_name (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (depend_name)" <> _]}
      assert_received {:mix_shell, :info, ["* app_name (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (package_name)" <> _]}
    end
  after
    purge [Depend_name.NoConflict.MixProject, Package_name.NoConflict.MixProject]
  end

  test "deps.get with incorrect version" do
    Mix.Project.push WithIncorrectDepVersion

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      message = ~s[Required version "> hello" for package ex_doc is incorrectly specified (from: mix.exs)]
      assert_raise Mix.Error, message, fn ->
        Mix.Task.run "deps.get"
      end
    end
  end

  test "deps.get with missing version" do
    Mix.Project.push WithMissingDepVersion

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"
      assert_received {:mix_shell, :info, ["\e[33mex_doc is missing its version requirement, use \">= 0.0.0\"" <> _]}
    end
  end

  test "deps.get with non matching requirement" do
    Mix.Project.push WithNonMatchingRequirement

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      message = ~r{No matching version for ex_doc ~> 100.0.0.*\n\nThe latest version is: 0.1.0}
      assert_raise Mix.Error, message, fn ->
        Mix.Task.run "deps.get"
      end
    end
  end

  test "deps.get with only matching pre requirement" do
    Mix.Project.push WithOnlyMatchingPreRequirement

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      message = ~r{No matching version for beta ~> 1.1.0.*\n\n.*pre-releases available.*\n\n  \* 1.1.0-beta}
      assert_raise Mix.Error, message, fn ->
        Mix.Task.run "deps.get"
      end
    end
  end

  defp old_lock_tuple(lock_tuple) do
    {elem(lock_tuple, 0), elem(lock_tuple, 1), elem(lock_tuple, 2), elem(lock_tuple, 3)}
  end

  defp rewrite_lock_in_old_format() do
    lock = Mix.Dep.Lock.read()
    old_lock = for {dep_key, dep_tuple} <- lock, into: %{} do
                 {dep_key, old_lock_tuple(dep_tuple)}
               end
    Mix.Dep.Lock.write(old_lock)
    old_lock
  end

  test "deps.get does not rewrite the lock file when deps are already present" do
    Mix.Project.push Simple

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"
      old_lock = rewrite_lock_in_old_format()

      Mix.Task.run "deps.get"
      assert old_lock == Mix.Dep.Lock.read()
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end

  test "deps.get does not rewrite the lock file when deps are not present" do
    Mix.Project.push Simple

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"
      old_lock = rewrite_lock_in_old_format()

      File.rm_rf!("deps")
      Mix.Task.run "deps.get"
      assert old_lock == Mix.Dep.Lock.read()
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end

  test "deps.update only rewrites given dependencies" do
    Mix.Project.push Simple

    in_tmp fn ->
      Hex.State.put(:home, System.cwd!)

      Mix.Task.run "deps.get"
      rewrite_lock_in_old_format()

      Mix.Task.run "deps.update", ["ex_doc"]

      new_lock = Mix.Dep.Lock.read()
      assert %{ecto: {:hex, :ecto, "0.2.0", _},
               postgrex: {:hex, :postgrex, "0.2.0", _}} = new_lock
      assert tuple_size(new_lock.ex_doc) > 4
    end
  after
    purge [Ecto.NoConflict.MixProject, Postgrex.NoConflict.MixProject,
           Ex_doc.NoConflict.MixProject]
  end
end
