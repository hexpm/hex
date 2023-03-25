# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Term do
  @moduledoc false

  alias Hex.Solver.{Constraint, PackageRange, Term}
  alias Hex.Solver.Constraints.Empty

  require Logger

  defstruct positive: true,
            package_range: nil,
            optional: false

  def relation(left, right, opts \\ []) do
    if Keyword.get(opts, :optionals, true) do
      case do_relation(left, right) do
        :disjoint -> if left.optional and not right.optional, do: :overlapping, else: :disjoint
        other -> if left.optional and right.optional, do: :disjoint, else: other
      end
    else
      do_relation(left, right)
    end
  end

  def do_relation(%Term{} = left, %Term{} = right) do
    left_constraint = constraint(left)
    right_constraint = constraint(right)

    cond do
      right.positive and left.positive ->
        cond do
          not compatible_package?(left, right) -> :disjoint
          Constraint.allows_all?(right_constraint, left_constraint) -> :subset
          not Constraint.allows_any?(left_constraint, right_constraint) -> :disjoint
          true -> :overlapping
        end

      right.positive and not left.positive ->
        cond do
          not compatible_package?(left, right) -> :overlapping
          Constraint.allows_all?(left_constraint, right_constraint) -> :disjoint
          true -> :overlapping
        end

      not right.positive and left.positive ->
        cond do
          not compatible_package?(left, right) -> :subset
          not Constraint.allows_any?(right_constraint, left_constraint) -> :subset
          Constraint.allows_all?(right_constraint, left_constraint) -> :disjoint
          true -> :overlapping
        end

      not right.positive and not left.positive ->
        cond do
          not compatible_package?(left, right) -> :overlapping
          Constraint.allows_all?(left_constraint, right_constraint) -> :subset
          true -> :overlapping
        end
    end
  end

  def intersect(%Term{} = left, %Term{} = right) do
    if compatible_package?(left, right) do
      optional = left.optional and right.optional

      cond do
        left.positive != right.positive ->
          positive = if left.positive, do: left, else: right
          negative = if left.positive, do: right, else: left

          constraint = Constraint.difference(constraint(positive), constraint(negative))
          non_empty_term(left, constraint, optional, true)

        left.positive and right.positive ->
          constraint = Constraint.intersect(constraint(left), constraint(right))
          non_empty_term(left, constraint, optional, true)

        not left.positive and not right.positive ->
          constraint = Constraint.union(constraint(left), constraint(right))
          non_empty_term(left, constraint, optional, false)
      end
    else
      nil
    end
  end

  def difference(%Term{} = left, %Term{} = right) do
    intersect(left, inverse(right))
  end

  def satisfies?(%Term{} = left, %Term{} = right, opts \\ []) do
    compatible_package?(left, right) and relation(left, right, opts) == :subset
  end

  def inverse(%Term{} = term) do
    %{term | positive: not term.positive}
  end

  def compatible_package?(%Term{package_range: %PackageRange{name: "$root"}}, %Term{}) do
    true
  end

  def compatible_package?(%Term{}, %Term{package_range: %PackageRange{name: "$root"}}) do
    true
  end

  def compatible_package?(%Term{package_range: left}, %Term{package_range: right}) do
    {left.repo, left.name} == {right.repo, right.name}
  end

  defp constraint(%Term{package_range: %PackageRange{constraint: constraint}}) do
    constraint
  end

  defp non_empty_term(_term, %Empty{}, _optional, _positive) do
    nil
  end

  defp non_empty_term(term, constraint, optional, positive) do
    %Term{
      package_range: %{term.package_range | constraint: constraint},
      positive: positive,
      optional: optional
    }
  end

  def to_string(%Term{package_range: package_range, positive: positive}) do
    "#{positive(positive)}#{package_range}"
  end

  defp positive(true), do: ""
  defp positive(false), do: "not "

  defimpl String.Chars do
    defdelegate to_string(term), to: Hex.Solver.Term
  end

  defimpl Inspect do
    def inspect(
          %Term{package_range: package_range, positive: positive, optional: optional},
          _opts
        ) do
      "#Term<#{positive(positive)}#{package_range}#{optional(optional)}>"
    end

    defp positive(true), do: ""
    defp positive(false), do: "not "

    defp optional(true), do: " (optional)"
    defp optional(false), do: ""
  end
end
