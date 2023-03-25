# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Registry do
  _ = """
  The registry is used by the solver to discover package versions and their dependencies.
  """

  @doc """
  Returns all versions of the given package sorted from lowest to highest or `:error` if the package
  does not exist.
  """
  @callback versions(Hex.Solver.repo(), Hex.Solver.package()) :: {:ok, [Version.t()]} | :error

  @doc """
  Returns all dependencies of the given package version or `:error` if the package or version
  does not exist.
  """
  @callback dependencies(Hex.Solver.repo(), Hex.Solver.package(), Version.t()) ::
              {:ok, [Hex.Solver.dependency()]} | :error

  @doc """
  Called when the solver first discovers a set of packages so that the registry can be lazily
  preloaded.
  """
  @callback prefetch([{Hex.Solver.repo(), Hex.Solver.package()}]) :: :ok
end
