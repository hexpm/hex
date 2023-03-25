# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Constraints.Range do
  @moduledoc false

  use Hex.Solver.Constraints.Impl

  alias Hex.Solver.Constraint
  alias Hex.Solver.Constraints.{Empty, Range, Union, Util, Version}

  # %Range{min: nil, max: nil} allows any version.
  # %Range{min: min, max: max, include_min: true, include_max: true}
  # when min == max is a single version.
  defstruct min: nil,
            max: nil,
            include_min: false,
            include_max: false

  def valid?(%Range{min: min, max: max, include_min: include_min, include_max: include_max}) do
    case version_compare(min, max) do
      :lt -> true
      :eq -> include_min and include_max
      :gt -> false
    end
  end

  def any?(%Range{min: nil, max: nil}), do: true
  def any?(%Range{}), do: false

  def empty?(%Range{}), do: false

  def allows?(%Range{} = range, %Elixir.Version{} = version) do
    compare_min = version_compare(range.min, version)

    if compare_min == :lt or (compare_min == :eq and range.include_min) do
      compare_max = version_compare(version, range.max)
      compare_max == :lt or (compare_max == :eq and range.include_max)
    else
      false
    end
  end

  def allows_all?(%Range{}, %Empty{}), do: true
  def allows_all?(%Range{} = range, %Elixir.Version{} = version), do: allows?(range, version)

  def allows_all?(%Range{} = left, %Range{} = right) do
    not allows_lower?(right, left) and not allows_higher?(right, left)
  end

  def allows_all?(%Range{} = range, %Union{ranges: ranges}) do
    Enum.all?(ranges, &allows_all?(range, &1))
  end

  def allows_any?(%Range{}, %Empty{}), do: true
  def allows_any?(%Range{} = range, %Elixir.Version{} = version), do: allows?(range, version)

  def allows_any?(%Range{} = left, %Range{} = right) do
    not strictly_lower?(right, left) and not strictly_higher?(right, left)
  end

  def allows_any?(%Range{} = range, %Union{ranges: ranges}) do
    Enum.any?(ranges, &allows_any?(range, &1))
  end

  def allows_lower?(%Range{} = left, %Range{} = right) do
    cond do
      left.min == nil ->
        right.min != nil

      right.min == nil ->
        false

      true ->
        case Version.compare(left.min, right.min) do
          :lt -> true
          :gt -> false
          :eq -> left.include_min and not right.include_min
        end
    end
  end

  def allows_higher?(%Range{} = left, %Range{} = right) do
    cond do
      left.max == nil ->
        right.max != nil

      right.max == nil ->
        false

      true ->
        case Version.compare(left.max, right.max) do
          :lt -> false
          :gt -> true
          :eq -> left.include_max and not right.include_max
        end
    end
  end

  def strictly_lower?(%Elixir.Version{} = left, %Elixir.Version{} = right) do
    Version.compare(left, right) == :lt
  end

  def strictly_lower?(%Range{} = range, %Elixir.Version{} = version) do
    range.max != nil and Version.compare(range.max, version) == :lt
  end

  def strictly_lower?(%Elixir.Version{} = version, %Range{} = range) do
    range.min != nil and Version.compare(version, range.min) == :lt
  end

  def strictly_lower?(%Range{} = left, %Range{} = right) do
    if left.max == nil or right.min == nil do
      false
    else
      case Version.compare(left.max, right.min) do
        :lt -> true
        :gt -> false
        :eq -> not left.include_max or not right.include_min
      end
    end
  end

  def strictly_higher?(left, right) do
    strictly_lower?(right, left)
  end

  def difference(%Range{} = range, %Empty{}) do
    range
  end

  def difference(%Range{} = range, %Elixir.Version{} = version) do
    cond do
      not allows?(range, version) ->
        range

      range.min == version ->
        %{range | include_min: false}

      range.max == version ->
        %{range | include_max: false}

      true ->
        %Union{
          ranges: [
            %{range | max: version, include_max: false},
            %{range | min: version, include_min: false}
          ]
        }
    end
  end

  def difference(%Range{} = left, %Range{} = right) do
    if allows_any?(left, right) do
      before_range =
        cond do
          not allows_lower?(left, right) ->
            nil

          left.min == right.min ->
            true = left.include_min and not right.include_min
            true = left.min != nil
            left.min

          true ->
            %Range{
              min: left.min,
              max: right.min,
              include_min: left.include_min,
              include_max: not right.include_min
            }
        end

      after_range =
        cond do
          not allows_higher?(left, right) ->
            nil

          left.max == right.max ->
            true = left.include_max and not right.include_max
            true = left.max != nil
            left.max

          true ->
            %Range{
              min: right.max,
              max: left.max,
              include_min: not right.include_max,
              include_max: left.include_max
            }
        end

      cond do
        before_range == nil and after_range == nil -> %Empty{}
        before_range == nil -> after_range
        after_range == nil -> before_range
        true -> %Union{ranges: [before_range, after_range]}
      end
    else
      left
    end
  end

  def difference(%Range{} = range, %Union{} = union) do
    {range, ranges} =
      Enum.reduce_while(union.ranges, {range, []}, fn union_range, {range, acc} ->
        cond do
          strictly_lower?(union_range, range) ->
            {:cont, {range, acc}}

          strictly_higher?(union_range, range) ->
            {:halt, {range, acc}}

          true ->
            case Constraint.difference(range, union_range) do
              %Empty{} -> {:halt, {range, %Empty{}}}
              %Union{ranges: [first, last]} -> {:cont, {last, [first | acc]}}
              constraint -> {:cont, {constraint, acc}}
            end
        end
      end)

    case ranges do
      %Empty{} -> %Empty{}
      [] -> range
      _ -> %Union{ranges: Enum.reverse([range | ranges])}
    end
  end

  def intersect(%Range{}, %Empty{}) do
    %Empty{}
  end

  def intersect(%Range{} = range, %Elixir.Version{} = version) do
    if allows?(range, version) do
      version
    else
      %Empty{}
    end
  end

  def intersect(%Range{} = left, %Range{} = right) do
    if strictly_lower?(left, right) or strictly_lower?(right, left) do
      %Empty{}
    else
      {intersect_min, intersect_include_min} =
        if allows_lower?(left, right) do
          {right.min, right.include_min}
        else
          {left.min, left.include_min}
        end

      {intersect_max, intersect_include_max} =
        if allows_higher?(left, right) do
          {right.max, right.include_max}
        else
          {left.max, left.include_max}
        end

      cond do
        intersect_min == nil and intersect_max == nil ->
          # Open range
          %Range{}

        intersect_min == intersect_max ->
          # There must be overlap since we already checked none of
          # the ranges are strictly lower
          true = intersect_include_min and intersect_include_max
          intersect_min

        true ->
          %Range{
            min: intersect_min,
            max: intersect_max,
            include_min: intersect_include_min,
            include_max: intersect_include_max
          }
      end
    end
  end

  def intersect(%Range{} = range, %Union{} = union) do
    Union.intersect(union, range)
  end

  def union(%Range{} = range, %Empty{}) do
    range
  end

  def union(%Range{} = range, %Elixir.Version{} = version) do
    if allows?(range, version) do
      range
    else
      case range do
        %Range{min: ^version} -> %{range | include_min: true}
        %Range{max: ^version} -> %{range | include_max: true}
        _ -> Util.union([range, version])
      end
    end
  end

  def union(%Range{} = left, %Range{} = right) do
    if not edges_touch?(left, right) and not Range.allows_any?(left, right) do
      Util.union([left, right])
    else
      min = if allows_lower?(left, right), do: left, else: right
      max = if allows_higher?(left, right), do: left, else: right

      %Range{
        min: min.min,
        max: max.max,
        include_min: min.include_min,
        include_max: max.include_max
      }
    end
  end

  def union(%Range{} = range, %Union{} = union) do
    Util.union([range, union])
  end

  defp edges_touch?(left, right) do
    (left.max != nil and left.max == right.min and (left.include_max or right.include_min)) or
      (left.min != nil and left.min == right.max and (left.include_min or right.include_max))
  end

  def compare(%Range{min: min, include_min: include_min}, %Elixir.Version{} = version) do
    if min == nil do
      :lt
    else
      case Version.compare(min, version) do
        :eq when include_min -> :eq
        :eq -> :gt
        :lt -> :lt
        :gt -> :gt
      end
    end
  end

  def compare(%Range{} = left, %Range{} = right) do
    cond do
      left.min == nil and right.min == nil ->
        :eq

      left.min == nil ->
        :lt

      right.min == nil ->
        :gt

      true ->
        left_include_min = left.include_min
        right_include_min = right.include_min

        case Version.compare(left.min, right.min) do
          :eq when left_include_min == right_include_min -> :eq
          :eq when left_include_min -> :lt
          :eq when right_include_min -> :gt
          :lt -> :lt
          :gt -> :gt
        end
    end
  end

  def compare(%Range{} = left, %Union{ranges: [right | _]}) do
    compare(left, right)
  end

  def single_version?(%Range{min: min, max: max, include_min: true, include_max: true}) do
    min == max
  end

  def single_version?(%Range{}) do
    false
  end

  defp version_compare(nil, _right), do: :lt
  defp version_compare(_left, nil), do: :lt
  defp version_compare(left, right), do: Version.compare(left, right)

  def normalize(%Range{min: version, max: version, include_min: true, include_max: true}),
    do: version

  def normalize(%Range{} = range), do: range
  def normalize(%Elixir.Version{} = version), do: version

  def to_string(%Range{min: nil, max: nil}) do
    "any"
  end

  def to_string(%Range{min: version, max: version, include_min: true, include_max: true}) do
    Kernel.to_string(version)
  end

  def to_string(%Range{
        min: %Elixir.Version{major: min_major, minor: min_minor, patch: 0, pre: pre},
        max: %Elixir.Version{major: max_major, minor: 0, patch: 0, pre: [0]},
        include_min: true,
        include_max: false
      })
      when min_major + 1 == max_major do
    "~> #{min_major}.#{min_minor}#{pre_to_string(pre)}"
  end

  def to_string(%Range{
        min: %Elixir.Version{major: min_major, minor: min_minor, patch: min_patch, pre: pre},
        max: %Elixir.Version{major: max_major, minor: max_minor, patch: 0, pre: [0]},
        include_min: true,
        include_max: false
      })
      when min_major == max_major and min_minor + 1 == max_minor do
    "~> #{min_major}.#{min_minor}.#{min_patch}#{pre_to_string(pre)}"
  end

  def to_string(%Range{min: min, max: max, include_min: include_min, include_max: include_max}) do
    min_string = if min, do: ">#{include(include_min)} #{min}"
    max_string = if max, do: "<#{include(include_max)} #{max}"

    if min_string && max_string do
      "#{min_string} and #{max_string}"
    else
      min_string || max_string
    end
  end

  defp pre_to_string([]), do: ""
  defp pre_to_string(pre), do: "-" <> Enum.join(pre, ".")

  defp include(true), do: "="
  defp include(false), do: ""

  defimpl String.Chars do
    defdelegate to_string(range), to: Hex.Solver.Constraints.Range
  end

  defimpl Inspect do
    def inspect(range, _opts) do
      "#Range<#{range}>"
    end
  end
end
