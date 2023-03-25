# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver do
  _ = """
  A version solver.
  """

  @type dependency() :: %{
          repo: repo(),
          name: package(),
          constraint: constraint(),
          optional: optional(),
          label: label(),
          dependencies: [dependency()]
        }
  @type locked() :: %{
          repo: repo(),
          name: package(),
          version: Version.t(),
          label: label()
        }
  @type repo() :: String.t() | nil
  @type package() :: String.t()
  @type label() :: String.t()
  @type optional() :: boolean()
  @type result() :: %{package() => {Version.t(), repo()}}
  @opaque constraint() :: Hex.Solver.Requirement.t()

  alias Hex.Solver.{Failure, Requirement, Solver}

  @doc """
  Runs the version solver.

  Takes a `Hex.Solver.Registry` implementation, a list of root dependencies, a list of locked
  package versions, and a list of packages that are overridden by the root dependencies.

  Depdendencies with sub-dependencies in the `:dependencies` map field are not expected to be
  packages in the registry. These dependencies are used to give better error messages for
  when there are multiple declarations of the dependency with conflicting version requirements.
  For example:

      * Top dependency 1
        * package ~> 1.0
      * Top dependency 2
        * package ~> 2.0

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
          {:ok, result()} | {:error, String.t()}
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
