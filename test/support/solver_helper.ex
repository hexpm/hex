defmodule HexTest.SolverHelper do
  alias Hex.Registry.Server, as: Registry

  def solve(dependencies, locked \\ []) do
    [dependencies, locked]
    |> Enum.concat()
    |> Enum.map(&to_string(elem(&1, 0)))
    |> Registry.prefetch()

    dependencies = dependencies(dependencies)
    locked = locked(locked)
    overridden = []

    case Hex.Solver.run(Registry, dependencies, locked, overridden) do
      {:ok, result} ->
        Map.new(result, fn {package, version} ->
          {String.to_atom(package), Version.to_string(version)}
        end)

      {:error, message} ->
        message
    end
  end

  defp dependencies(dependencies) do
    Enum.map(dependencies, fn
      {package, requirement} ->
        package = to_string(package)
        {package, Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"), false, package}

      {package, requirement, opts} ->
        package = to_string(package)
        optional = Keyword.get(opts, :optional, false)
        app = Keyword.get(opts, :app, false)
        {package, Hex.Solver.parse_constraint!(requirement || ">= 0.0.0"), optional, app}
    end)
  end

  defp locked(dependencies) do
    Enum.map(dependencies, fn {package, requirement} ->
      {to_string(package), Hex.Solver.parse_constraint!(requirement || ">= 0.0.0")}
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
