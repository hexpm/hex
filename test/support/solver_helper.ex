defmodule HexTest.SolverHelper do
  alias Hex.Registry.Server, as: Registry

  def solve(deps, locked \\ []) do
    dependencies = dependencies(deps)
    locked = locked(locked)
    overridden = overridden(deps)

    (dependencies ++ locked)
    |> Enum.map(fn %{repo: repo, name: name} -> {repo, name} end)
    |> Registry.prefetch()

    case Hex.Solver.run(Registry, dependencies, locked, overridden) do
      {:ok, result} ->
        Map.new(result, fn
          {package, {version, nil}} ->
            {String.to_atom(package), to_string(version)}

          {package, {version, repo}} ->
            {{repo, String.to_atom(package)}, to_string(version)}
        end)

      {:error, message} ->
        message
    end
  end

  defp dependencies(dependencies) do
    Enum.map(dependencies, fn
      {package, requirement} ->
        %{
          repo: nil,
          name: to_string(package),
          constraint: Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"),
          optional: false,
          label: package,
          dependencies: []
        }

      {package, requirement, opts} ->
        repo = Keyword.get(opts, :repo)
        optional = Keyword.get(opts, :optional, false)
        app = Keyword.get(opts, :app, false)

        %{
          repo: repo,
          name: to_string(package),
          constraint: Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"),
          optional: optional,
          label: app,
          dependencies: []
        }
    end)
  end

  defp locked(dependencies) do
    Enum.map(dependencies, fn
      {package, requirement} ->
        %{
          repo: nil,
          name: to_string(package),
          version: Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"),
          label: package
        }

      {package, requirement, opts} ->
        repo = Keyword.get(opts, :repo)
        app = Keyword.get(opts, :app, false)

        %{
          repo: repo,
          name: to_string(package),
          version: Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"),
          label: app
        }
    end)
  end

  defp overridden(dependencies) do
    Enum.flat_map(dependencies, fn
      {_package, _requirement} ->
        []

      {package, _requirement, opts} ->
        if opts[:override] do
          [package]
        else
          []
        end
    end)
  end

  def setup_registry(path, registry) do
    registry = expand_versions(registry)
    HexTest.Case.create_test_registry(path, registry)
    Registry.close()
    Registry.open(registry_path: path)
  end

  def expand_versions(registry) do
    Enum.flat_map(registry, fn {repo, package, versions, deps} ->
      Enum.map(versions, fn version ->
        {repo, package, version, deps}
      end)
    end)
  end
end
