# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Constraints.Empty do
  @moduledoc false

  use Hex.Solver.Constraints.Impl

  alias Hex.Solver.Constraint
  alias Hex.Solver.Constraints.Empty

  defstruct []

  def any?(%Empty{}), do: false

  def empty?(%Empty{}), do: true

  def allows?(%Empty{}, %Version{}), do: false

  def allows_any?(%Empty{}, constraint), do: Constraint.empty?(constraint)

  def allows_all?(%Empty{}, constraint), do: Constraint.empty?(constraint)

  def difference(%Empty{}, _constraint), do: %Empty{}

  def intersect(%Empty{}, _constraint), do: %Empty{}

  def union(%Empty{}, constraint), do: constraint

  def compare(left, right) do
    raise FunctionClauseError,
      module: __MODULE__,
      function: :compare,
      arity: 2,
      kind: :def,
      args: [left, right],
      clauses: []
  end

  def to_string(%Empty{}) do
    "empty"
  end

  defimpl String.Chars do
    defdelegate to_string(empty), to: Hex.Solver.Constraints.Empty
  end

  defimpl Inspect do
    def inspect(_, _opts) do
      "#Empty<>"
    end
  end
end
