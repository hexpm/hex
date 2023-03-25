# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Constraints.Union do
  @moduledoc false

  use Hex.Solver.Constraints.Impl

  alias Hex.Solver.Constraint
  alias Hex.Solver.Constraints.{Empty, Range, Union, Util, Version}

  # We will always work under the following assumptions for unions:
  # * Minimum of two elements
  # * Only %Range{} and %Version{}, no %Empty{}
  # * No "any range" (%Range{min: nil, max: nil})
  # * Elements sorted by their minimum version
  # * Ranges do not overlap

  defstruct ranges: []

  def empty?(%Union{}), do: false

  def any?(%Union{}), do: false

  def allows?(%Union{ranges: ranges}, %Elixir.Version{} = version) do
    Enum.any?(ranges, &Constraint.allows?(&1, version))
  end

  def allows_all?(%Union{ranges: left}, %Union{ranges: right}) do
    do_allows_all?(left, right)
  end

  def allows_all?(%Union{ranges: left}, right) do
    do_allows_all?(left, [right])
  end

  # We can recurse left and right together since they are
  # sorted on minimum version
  defp do_allows_all?([left | lefts], [right | rights]) do
    if Constraint.allows_all?(left, right) do
      do_allows_all?([left | lefts], rights)
    else
      do_allows_all?(lefts, [right | rights])
    end
  end

  defp do_allows_all?(_lefts, []), do: true
  defp do_allows_all?([], _rights), do: false

  def allows_any?(%Union{}, %Empty{}), do: true

  def allows_any?(%Union{} = left, right) do
    do_allows_any?(to_ranges(left), to_ranges(right))
  end

  # We can recurse left and right together since they are
  # sorted on minimum version
  defp do_allows_any?([left | lefts], [right | rights]) do
    cond do
      Range.allows_any?(left, right) ->
        true

      # Move forward with the range with the lower max value
      Range.allows_higher?(right, left) ->
        do_allows_any?(lefts, [right | rights])

      true ->
        do_allows_any?([left | lefts], rights)
    end
  end

  defp do_allows_any?(_lefts, _rights), do: false

  def difference(%Union{} = left, right) do
    do_difference(to_ranges(left), to_ranges(right), [])
  end

  defp do_difference(lefts, [], acc) do
    # If there are no more "right" ranges, none of the rest needs to
    # be subtracted and can be added as-is
    Util.from_list(Enum.reverse(acc) ++ lefts)
  end

  defp do_difference([], _rights, acc) do
    Util.from_list(Enum.reverse(acc))
  end

  defp do_difference([left | lefts], [right | rights], acc) do
    cond do
      Range.strictly_lower?(right, left) ->
        do_difference([left | lefts], rights, acc)

      Range.strictly_higher?(right, left) ->
        do_difference(lefts, [right | rights], [left | acc])

      true ->
        # Left and right overlaps
        case maybe_to_range(Range.difference(left, right)) do
          %Union{ranges: [first, last]} ->
            # If right splits left in half, we only need to check future ranges
            # against the latter half
            do_difference([last | lefts], rights, [first | acc])

          %Range{} = range ->
            # Move the constraint with the lower max value forward. Ensures
            # we keep both lists in sync as much as possible and that large
            # ranges have a chance to subtract or be subtracted of multiple
            # small ranges
            if Range.allows_higher?(range, right) do
              do_difference([range | lefts], rights, acc)
            else
              do_difference(lefts, [right | rights], [range | acc])
            end

          %Empty{} ->
            do_difference(lefts, [right | rights], acc)
        end
    end
  end

  def intersect(%Union{} = left, right) do
    do_intersect(to_ranges(left), to_ranges(right), [])
  end

  defp do_intersect([left | lefts], [right | rights], acc) do
    acc =
      case Constraint.intersect(left, right) do
        %Empty{} -> acc
        intersection -> [intersection | acc]
      end

    if Range.allows_higher?(right, left) do
      do_intersect(lefts, [right | rights], acc)
    else
      do_intersect([left | lefts], rights, acc)
    end
  end

  defp do_intersect(_lefts, [], acc), do: Util.from_list(Enum.reverse(acc))
  defp do_intersect([], _rights, acc), do: Util.from_list(Enum.reverse(acc))

  def union(%Union{} = left, right) do
    Util.union([left, right])
  end

  def compare(%Union{ranges: [range | _]}, right) do
    Constraint.compare(range, right)
  end

  defp to_ranges(%Empty{}), do: []
  defp to_ranges(%Elixir.Version{} = version), do: [Version.to_range(version)]
  defp to_ranges(%Range{} = range), do: [range]
  defp to_ranges(%Union{ranges: ranges}), do: Enum.map(ranges, &Version.to_range/1)

  defp maybe_to_range(%Elixir.Version{} = version), do: Version.to_range(version)
  defp maybe_to_range(other), do: other

  def to_string(%Union{ranges: ranges}) do
    Enum.map_join(ranges, " or ", &Kernel.to_string/1)
  end

  defimpl String.Chars do
    defdelegate to_string(union), to: Hex.Solver.Constraints.Union
  end

  defimpl Inspect do
    def inspect(union, _opts) do
      "#Union<#{union}>"
    end
  end
end
