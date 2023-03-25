# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Constraints.Util do
  @moduledoc false

  alias Hex.Solver.{Constraint, Util}
  alias Hex.Solver.Constraints.{Empty, Range, Union, Version}

  def any(), do: %Range{}

  def from_list([]), do: %Empty{}
  def from_list([single]), do: Range.normalize(single)
  def from_list(acc), do: %Union{ranges: Enum.map(acc, &Range.normalize/1)}

  def union(list) do
    list = flatten(list)

    cond do
      list == [] ->
        %Empty{}

      Enum.any?(list, &Constraint.any?/1) ->
        %Range{}

      true ->
        list
        |> Enum.sort(Util.compare(Constraint))
        |> Enum.reduce([], fn constraint, acc ->
          previous = List.first(acc)

          cond do
            acc == [] ->
              [constraint]

            not Constraint.allows_any?(previous, constraint) and
                not adjacent?(previous, constraint) ->
              [constraint | acc]

            true ->
              # Merge this constraint with previous, but only if they touch
              List.replace_at(acc, 0, Constraint.union(previous, constraint))
          end
        end)
        |> Enum.reverse()
        |> from_list()
    end
  end

  defp flatten(list) do
    Enum.flat_map(list, fn
      %Union{ranges: ranges} -> ranges
      %Empty{} -> []
      other -> [other]
    end)
  end

  @doc """
  Returns `true` if `left` is immediately next to, but not overlapping, `right`.

  Assumes `left` is lower than `right`.
  """
  def adjacent?(left, right) do
    left = Version.to_range(left)
    right = Version.to_range(right)

    left.max == right.min and
      ((left.include_max and not right.include_min) or
         (not left.include_max and right.include_min))
  end

  def from_bounds(%Elixir.Version{} = lower, %Elixir.Version{} = upper) do
    case Version.compare(lower, upper) do
      :eq -> lower
      :lt -> %Range{min: lower, max: upper, include_min: true, include_max: true}
    end
  end

  def from_bounds(%Elixir.Version{} = lower, nil), do: %Range{min: lower, include_min: true}
  def from_bounds(nil, %Elixir.Version{} = upper), do: %Range{max: upper, include_max: true}
  def from_bounds(nil, nil), do: %Range{}
end
