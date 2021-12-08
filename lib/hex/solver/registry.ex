# Vendored from hex_solver v0.1.0 (d6269a8), do not edit manually

defmodule Hex.Solver.Registry do
  _ = """
  The registry is used by the solver to discover package versions and their dependencies.
  """

  @doc """
  Returns all versions of the given package or `:error` if the package does not exist.
  """
  @callback versions(Hex.Solver.package()) :: {:ok, [Version.t()]} | :error

  @doc """
  Returns all dependencies of the given package version or `:error` if the package or version
  does not exist.
  """
  @callback dependencies(Hex.Solver.package(), Version.t()) ::
              {:ok, [Hex.Solver.dependency()]} | :error

  @doc """
  Called when the solver first discovers a set of packages so that the registry can be lazily
  preloaded.
  """
  @callback prefetch([Hex.Solver.package()]) :: :ok
end
