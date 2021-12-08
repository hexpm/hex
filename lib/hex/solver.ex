# Vendored from hex_solver v0.1.0 (d6269a8), do not edit manually

defmodule Hex.Solver do
  _ = """
  A version solver.
  """

  @type dependency() :: {package(), constraint(), optional(), label()}
  @type locked() :: {package(), Version.t()}
  @type package() :: String.t()
  @type label() :: String.t()
  @type optional() :: boolean()
  @opaque constraint() :: Hex.Solver.Requirement.t()

  alias Hex.Solver.{Failure, Requirement, Solver}

  @doc """
  Runs the version solver.

  Takes a `Hex.Solver.Registry` implementation, a list of root dependencies, a list of locked
  package versions, and a list of packages that are overridden by the root dependencies.

  Locked dependencies are treated as optional dependencies with a single version as
  their constraint.

  The overrides are a set of labels. If a dependency with a matching label is declared the
  solver will ignore that dependency unless it's a root dependency.

  Returns a map of packages and their selected versions or a human readable explanation of
  why a solution could not be found.

  ## Options

    * `:ansi` - If `true` adds ANSI formatting to the failure message (Default: `false`)

  """
  @spec run(module(), [dependency()], [locked()], [label()], ansi: boolean()) ::
          {:ok, %{package() => Version.t()}} | {:error, String.t()}
  def run(registry, dependencies, locked, overrides, opts \\ []) do
    case Solver.run(registry, dependencies, locked, overrides) do
      {:ok, solution} -> {:ok, Map.drop(solution, ["$root", "$lock"])}
      {:error, incompatibility} -> {:error, Failure.write(incompatibility, opts)}
    end
  end

  @doc """
  Parses or converts a SemVer version or Elixir version requirement to an internal solver constraint
  that can be returned by the `Hex.Solver.Registry` or passed to `Hex.Solver.run/4`.
  """
  @spec parse_constraint(String.t() | Version.t() | Version.Requirement.t()) ::
          {:ok, constraint()} | :error
  def parse_constraint(string) do
    Requirement.to_constraint(string)
  end

  @doc """
  Parses or converts a SemVer version or Elixir version requirement to an internal solver constraint
  that can be returned by the `Hex.Solver.Registry` or passed to `Hex.Solver.run/4`.
  """
  @spec parse_constraint!(String.t() | Version.t() | Version.Requirement.t()) :: constraint()
  def parse_constraint!(string) do
    Requirement.to_constraint!(string)
  end
end
