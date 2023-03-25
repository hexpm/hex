defmodule Hex.MixTaskTest do
  use HexTest.IntegrationCase

  defmodule Simple do
    def project do
      [
        app: :simple,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ecto, "0.2.0"}
        ]
      ]
    end
  end

  defmodule SimpleOld do
    def project do
      [
        app: :simple,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ecto, "~> 0.2.1"}
        ]
      ]
    end
  end

  defmodule EctoDep do
    def project do
      [
        app: :simple,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ecto, "~> 0.2.0"}
        ]
      ]
    end
  end

  defmodule Override do
    def project do
      [
        app: :override,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ecto, "0.2.0"},
          {:ex_doc, "~> 0.1.0", override: true}
        ]
      ]
    end
  end

  defmodule NonHexDep do
    def project do
      [
        app: :non_hex_dep,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:has_hex_dep, path: fixture_path("has_hex_dep")}
        ]
      ]
    end
  end

  defmodule EctoPathDep do
    def project do
      [
        app: :ecto_path_dep,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:postgrex, ">= 0.0.0"},
          {:ecto, path: fixture_path("ecto")}
        ]
      ]
    end
  end

  defmodule EctoPathDepConflict do
    def project do
      [
        app: :ecto_path_dep,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:postgrex, "0.2.1"},
          {:ecto, path: fixture_path("ecto")}
        ]
      ]
    end
  end

  defmodule EctoPathDepAppConflict do
    def project do
      [
        app: :ecto_path_dep,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:postgrex_conflict, ">= 0.0.0", hex: :postgrex},
          {:ecto, path: fixture_path("ecto")}
        ]
      ]
    end
  end

  defmodule OverrideWithPath do
    def project do
      [
        app: :override_with_path,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:postgrex, ">= 0.0.0"},
          {:ex_doc, path: fixture_path("ex_doc"), override: true}
        ]
      ]
    end
  end

  defmodule OverrideTwoLevelsWithPath do
    def project do
      [
        app: :override_two_levels_with_path,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:phoenix, ">= 0.0.0"},
          {:ex_doc, path: fixture_path("ex_doc"), override: true}
        ]
      ]
    end
  end

  defmodule OverrideWithPathParent do
    def project do
      [
        app: :override_with_path_parent,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:override_with_path, path: fixture_path("override_with_path")}
        ]
      ]
    end
  end

  defmodule Optional do
    def project do
      [
        app: :optional,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:only_doc, ">= 0.0.0"}
        ]
      ]
    end
  end

  defmodule WithOptional do
    def project do
      [
        app: :with_optional,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:only_doc, ">= 0.0.0"},
          {:ex_doc, "0.0.1"}
        ]
      ]
    end
  end

  defmodule WithPackageName do
    def project do
      [
        app: :with_package_name,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:app_name, ">= 0.0.0", hex: :package_name}
        ]
      ]
    end
  end

  defmodule WithDependName do
    def project do
      [
        app: :with_depend_name,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:depend_name, ">= 0.0.0"}
        ]
      ]
    end
  end

  defmodule WithIncorrectDepVersion do
    def project do
      [
        app: :with_incorrect_dep_version,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ex_doc, "> hello"}
        ]
      ]
    end
  end

  defmodule WithMissingDepVersion do
    def project do
      [
        app: :with_missing_dep_version,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ex_doc, []}
        ]
      ]
    end
  end

  defmodule WithNonMatchingRequirement do
    def project do
      [
        app: :with_non_matching_requirement,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ex_doc, "~> 100.0.0"}
        ]
      ]
    end
  end

  defmodule WithOnlyMatchingPreRequirement do
    def project do
      [
        app: :with_only_matching_pre_requirement,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:beta, "~> 1.1.0"}
        ]
      ]
    end
  end

  defmodule DependsOnEctoSQL do
    def project do
      [
        app: :depends_on_ecto_sql,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:ecto_sql, "~> 3.3"},
          {:ecto_enum, "1.4.0"}
        ]
      ]
    end
  end

  defmodule DependsOnSponsored do
    def project do
      [
        app: :depends_on_sponsored,
        version: "0.1.0",
        consolidate_protocols: false,
        deps: [
          {:sponsored, "0.1.0"}
        ]
      ]
    end
  end

  defp reset_code_paths(fun) do
    path = :code.get_path()

    try do
      fun.()
    after
      :code.add_pathsz(path)
    end
  end

  defp deps_compile() do
    reset_code_paths(fn -> Mix.Task.run("deps.compile") end)
  end

  defp compile() do
    reset_code_paths(fn -> Mix.Task.run("compile") end)
  end

  test "deps.get" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting ecto (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}

      deps_compile()
      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* ecto 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (ecto)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* postgrex 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (postgrex)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* ex_doc 0.0.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.0.1 (ex_doc)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  test "deps.get with lock" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")
      Mix.Task.clear()

      Mix.Task.run("deps.get")
      deps_compile()
      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* ecto 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* postgrex 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc 0.0.1 (Hex package)" <> _]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  test "deps.update" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      # `deps.get` to set up lock
      Mix.Task.run("deps.get")

      purge([
        Ecto.NoConflict.MixProject,
        Postgrex.NoConflict.MixProject,
        Ex_doc.NoConflict.MixProject
      ])

      HexTest.Case.clear_cache()
      Mix.Project.pop()
      Mix.Project.push(SimpleOld)

      Mix.Task.run("deps.update", ["ecto"])

      assert_received {:mix_shell, :info, ["* Updating ecto (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Updating postgrex (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Updating ex_doc (Hex package)"]}

      deps_compile()
      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* ecto 0.2.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.1 (ecto)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* postgrex 0.2.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.1 (postgrex)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* ex_doc 0.1.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (ex_doc)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.get with old format, string, single line manifest file" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      manifest =
        "ecto,0.2.0,0b6d6e0d9ef90f55dad224c59cff751a445f9b3e5fcfe5d31aa0e964e1d7e3de,hexpm"

      File.write!("mix.lock", ~s(%{"ecto": {:hex, :ecto, "0.2.0"}}))
      File.mkdir_p!("deps/ecto")
      File.write!("deps/ecto/.hex", manifest)
      Mix.Task.run("deps.get", [])

      assert_received {:mix_shell, :info, ["  ecto 0.2.0"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.get with old format, string, multi line manifest file" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      manifest =
        "ecto,0.2.0,0b6d6e0d9ef90f55dad224c59cff751a445f9b3e5fcfe5d31aa0e964e1d7e3de,hexpm\n"

      File.write!("mix.lock", ~s(%{"ecto": {:hex, :ecto, "0.2.0"}}))
      File.mkdir_p!("deps/ecto")
      File.write!("deps/ecto/.hex", manifest)
      Mix.Task.run("deps.get", [])

      assert_received {:mix_shell, :info, ["  ecto 0.2.0"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.get with 1.0 manifest file" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      manifest_map = %{
        name: "ecto",
        version: "0.2.0",
        inner_checksum: "0b6d6e0d9ef90f55dad224c59cff751a445f9b3e5fcfe5d31aa0e964e1d7e3de",
        repo: "hexpm",
        managers: []
      }

      manifest = :erlang.term_to_binary({{:hex, 1, 0}, manifest_map})

      File.write!("mix.lock", ~s(%{"ecto": {:hex, :ecto, "0.2.0"}}))
      File.mkdir_p!("deps/ecto")
      File.write!("deps/ecto/.hex", manifest)
      Mix.Task.run("deps.get", [])

      assert_received {:mix_shell, :info, ["  ecto 0.2.0"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.update locked dependency with minimal lock file" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      File.write!("mix.lock", ~s(%{"ecto": {:hex, :ecto, "0.2.0"}}))
      Mix.Task.run("deps.update", ["ecto"])

      assert_received {:mix_shell, :info, ["  ecto 0.2.0 => 0.2.1"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.update locked dependency with old lockfile" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      File.write!("mix.lock", ~s(%{"ecto": {:hex, :ecto, "0.2.0", "CHECKSUM", [:mix]}}))
      Mix.Task.run("deps.update", ["ecto"])

      assert_received {:mix_shell, :info, ["  ecto 0.2.0 => 0.2.1"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.update locked dependency with new lockfile" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      File.write!(
        "mix.lock",
        ~s(%{"ecto": {:hex, :ecto, "0.2.0", "CHECKSUM", [:mix], [], "hexpm"}})
      )

      Mix.Task.run("deps.update", ["ecto"])

      assert_received {:mix_shell, :info, ["  ecto 0.2.0 => 0.2.1"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.update locked dependency from git" do
    Mix.Project.push(EctoDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      File.write!(
        "mix.lock",
        ~s(%{"ecto": {:git, "https://github.com/elixi-ecto/ecto", "CHECKSUM", []}})
      )

      Mix.Task.run("deps.update", ["ecto"])

      assert_received {:mix_shell, :info, ["  ecto 0.2.1"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      Sample.Fixture.MixProject
    ])
  end

  test "deps.get with override" do
    Mix.Project.push(Override)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")
      deps_compile()
      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* ecto 0.2.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (ecto)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* postgrex 0.2.1 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.1 (postgrex)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}

      assert_received {:mix_shell, :info, ["* ex_doc 0.1.0 (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (ex_doc)" <> _]}
      assert_received {:mix_shell, :info, ["  ok"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  test "deps.get with non hex dependency that has hex dependency" do
    Mix.Project.push(NonHexDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting ecto (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject,
      HasHexDep.Fixture.MixProject
    ])
  end

  test "converged hex dependency considers all requirements" do
    Mix.Project.push(EctoPathDep)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}

      assert %{postgrex: {:hex, :postgrex, "0.2.0", _, _, _, "hexpm", _}} = Mix.Dep.Lock.read()
    end)
  after
    purge([Ecto.Fixture.MixProject, Postgrex.NoConflict.MixProject, Ex_doc.NoConflict.MixProject])
  end

  test "converged hex dependency considers all requirements and creates conflict" do
    Mix.Project.push(EctoPathDepConflict)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      assert_raise(Mix.Error, "Hex dependency resolution failed", fn ->
        Mix.Task.run("deps.get")
      end)

      solver_output = """
      Because "your app" depends on "ecto" which depends on "postgrex 0.2.0", "postgrex 0.2.0" is required.
      So, because "your app" depends on "postgrex 0.2.1", version solving failed.\
      """

      assert_received {:mix_shell, :info, [^solver_output]}
    end)
  after
    purge([Ecto.Fixture.MixProject, Postgrex.NoConflict.MixProject, Ex_doc.NoConflict.MixProject])
  end

  test "converged hex dependency considers all requirements and creates app conflict" do
    Mix.Project.push(EctoPathDepAppConflict)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      assert_raise(
        Mix.Error,
        ~r"Conflicting OTP application names in dependency definition of \"postgrex\"",
        fn -> Mix.Task.run("deps.get") end
      )
    end)
  after
    purge([Ecto.Fixture.MixProject, Postgrex.NoConflict.MixProject, Ex_doc.NoConflict.MixProject])
  end

  test "do not fetch git children of hex dependencies" do
    Mix.Project.push(SimpleOld)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting ecto (Hex package)"]}

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* ecto (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* sample" <> _]}
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  test "override hex dependency with path dependency" do
    Mix.Project.push(OverrideWithPath)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* postgrex (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc" <> _]}

      assert %{postgrex: {:hex, :postgrex, "0.2.1", _, _, _, "hexpm", _}} = Mix.Dep.Lock.read()
    end)
  after
    purge([Postgrex.NoConflict.MixProject, ExDoc.Fixture.MixProject])
  end

  test "override hex dependency two levels down with path dependency" do
    Mix.Project.push(OverrideTwoLevelsWithPath)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* phoenix (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* postgrex (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc" <> _]}

      assert %{
               phoenix: {:hex, :phoenix, "0.0.1", _, _, _, "hexpm", _},
               postgrex: {:hex, :postgrex, "0.2.1", _, _, _, "hexpm", _}
             } = Mix.Dep.Lock.read()
    end)
  after
    purge([
      Phoenix.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      ExDoc.Fixture.MixProject
    ])
  end

  test "override hex dependency with path dependency from dependency" do
    Mix.Project.push(OverrideWithPathParent)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting postgrex (Hex package)"]}

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* postgrex (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc" <> _]}

      assert %{postgrex: {:hex, :postgrex, "0.2.1", _, _, _, "hexpm", _}} = Mix.Dep.Lock.read()
    end)
  after
    purge([
      OverrideWithPath.Fixture.MixProject,
      ExDoc.Fixture.MixProject,
      Postgrex.NoConflict.MixProject
    ])
  end

  test "optional dependency" do
    Mix.Project.push(Optional)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting only_doc (Hex package)"]}
      refute_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* only_doc (Hex package)" <> _]}
      refute_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
    end)
  after
    purge([Only_doc.NoConflict.MixProject, Ex_doc.NoConflict.MixProject])
  end

  test "with optional dependency" do
    Mix.Project.push(WithOptional)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting only_doc (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting ex_doc (Hex package)"]}

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* only_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["* ex_doc (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.0.1 (ex_doc)" <> _]}
    end)
  after
    purge([Only_doc.NoConflict.MixProject, Ex_doc.NoConflict.MixProject])
  end

  test "with package name" do
    Mix.Project.push(WithPackageName)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting app_name (Hex package)"]}

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* app_name (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (package_name)" <> _]}
    end)
  after
    purge([Package_name.NoConflict.MixProject])
  end

  test "with depend name" do
    Mix.Project.push(WithDependName)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting depend_name (Hex package)"]}
      assert_received {:mix_shell, :info, ["* Getting app_name (Hex package)"]}

      Mix.Task.run("deps")

      assert_received {:mix_shell, :info, ["* depend_name (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.2.0 (depend_name)" <> _]}
      assert_received {:mix_shell, :info, ["* app_name (Hex package)" <> _]}
      assert_received {:mix_shell, :info, ["  locked at 0.1.0 (package_name)" <> _]}
    end)
  after
    purge([Depend_name.NoConflict.MixProject, Package_name.NoConflict.MixProject])
  end

  test "deps.get with incorrect version" do
    Mix.Project.push(WithIncorrectDepVersion)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      message =
        ~s[Required version "> hello" for package ex_doc is incorrectly specified (from: mix.exs)]

      assert_raise Mix.Error, message, fn ->
        Mix.Task.run("deps.get")
      end
    end)
  end

  test "deps.get with missing version" do
    Mix.Project.push(WithMissingDepVersion)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info,
                       ["\e[33mex_doc is missing its version requirement, use \">= 0.0.0\"" <> _]}
    end)
  end

  defp old_lock_tuple(lock_tuple) do
    {elem(lock_tuple, 0), elem(lock_tuple, 1), elem(lock_tuple, 2), elem(lock_tuple, 3)}
  end

  defp rewrite_lock_in_old_format() do
    lock = Mix.Dep.Lock.read()

    old_lock =
      for {dep_key, dep_tuple} <- lock, into: %{} do
        {dep_key, old_lock_tuple(dep_tuple)}
      end

    Mix.Dep.Lock.write(old_lock)
    old_lock
  end

  test "deps.get populates lock" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())
      Mix.Task.run("deps.get")

      assert %{ecto: {:hex, :ecto, "0.2.0", old_checksum, [:mix], deps, "hexpm", new_checksum}} =
               Mix.Dep.Lock.read()

      assert String.length(new_checksum) == 64
      assert String.length(old_checksum) == 64

      assert deps == [
               {:ex_doc, "~> 0.0.1", [hex: :ex_doc, repo: "hexpm", optional: false]},
               {:postgrex, "~> 0.2.0", [hex: :postgrex, repo: "hexpm", optional: false]}
             ]
    end)
  end

  test "deps.get does not rewrite the lock file when deps are already present" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")
      old_lock = rewrite_lock_in_old_format()

      Mix.Task.run("deps.get")
      assert old_lock == Mix.Dep.Lock.read()
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  test "deps.get does not rewrite the lock file when deps are not present" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")
      old_lock = rewrite_lock_in_old_format()

      File.rm_rf!("deps")
      Mix.Task.run("deps.get")
      assert old_lock == Mix.Dep.Lock.read()
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  # The introduction of outer_checksum forces all locks to rewrite
  @tag :skip
  test "deps.update only rewrites given dependencies" do
    Mix.Project.push(Simple)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Task.run("deps.get")
      rewrite_lock_in_old_format()

      Mix.Task.run("deps.update", ["ex_doc"])

      assert %{
               ecto: {:hex, :ecto, "0.2.0", _},
               ex_doc: {:hex, :ex_doc, "0.0.1", _, [:mix], _, _, _},
               postgrex: {:hex, :postgrex, "0.2.0", _}
             } = Mix.Dep.Lock.read()
    end)
  after
    purge([
      Ecto.NoConflict.MixProject,
      Postgrex.NoConflict.MixProject,
      Ex_doc.NoConflict.MixProject
    ])
  end

  if Version.match?(System.version(), "< 1.12.0-dev") do
    @tag :skip
  end

  test "do not raise :divergedreq when parent changes requirement and child changes version" do
    Mix.Project.push(DependsOnEctoSQL)

    in_tmp(fn ->
      Hex.State.put(:cache_home, File.cwd!())

      Mix.Dep.Lock.write(%{
        ecto_sql: {:hex, :ecto_sql, "3.3.2"},
        ecto: {:hex, :ecto, "3.3.1"}
      })

      Mix.Task.run("deps.get")
      compile()
      Mix.Task.run("deps.update", ["ecto_sql"])
    end)
  after
    purge([
      Ecto.SQL_3_3_2.Fixture.MixProject,
      Ecto.SQL_3_3_3.Fixture.MixProject,
      Ecto.Enum_1_4_0.Fixture.MixProject,
      Ecto_3_3_1.Fixture.MixProject,
      Ecto_3_3_2.Fixture.MixProject
    ])
  end

  test "prints a sponsors tip when updating or adding a package with sponsor link" do
    Mix.Project.push(DependsOnSponsored)

    in_tmp("sponsor_tmp", fn ->
      Hex.State.put(:cache_home, tmp_path())
      Mix.Task.run("deps.get")

      assert_received {:mix_shell, :info, ["* Getting sponsored (Hex package)"]}

      assert_received {:mix_shell, :info,
                       [
                         "You have added/upgraded packages you could " <>
                           "sponsor, run `mix hex.sponsor` to learn more"
                       ]}
    end)
  end
end
