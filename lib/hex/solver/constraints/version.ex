# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Constraints.Version do
  @moduledoc false

  use Hex.Solver.Constraints.Impl, for: Version

  import Kernel, except: [match?: 2]

  alias Hex.Solver.Constraint
  alias Hex.Solver.Constraints.{Empty, Range, Union, Util}

  def any?(%Version{}), do: false

  def empty?(%Version{}), do: false

  def allows?(%Version{} = left, %Version{} = right), do: left == right

  def allows_any?(%Version{}, %Empty{}), do: true
  def allows_any?(%Version{} = left, right), do: Constraint.allows?(right, left)

  def allows_all?(%Version{}, %Empty{}) do
    true
  end

  def allows_all?(%Version{} = left, %Version{} = right) do
    left == right
  end

  def allows_all?(%Version{}, %Range{min: nil}) do
    false
  end

  def allows_all?(%Version{}, %Range{max: nil}) do
    false
  end

  def allows_all?(%Version{} = version, %Range{
        min: min,
        max: max,
        include_min: true,
        include_max: true
      }) do
    version == min and version == max
  end

  def allows_all?(%Version{}, %Range{}) do
    false
  end

  def allows_all?(%Version{} = version, %Union{ranges: ranges}) do
    Enum.all?(ranges, &allows_all?(version, &1))
  end

  def difference(%Version{} = version, constraint) do
    if Constraint.allows?(constraint, version), do: %Empty{}, else: version
  end

  def intersect(%Version{} = version, constraint) do
    if Constraint.allows?(constraint, version), do: version, else: %Empty{}
  end

  def union(%Version{} = version, constraint) do
    if Constraint.allows?(constraint, version) do
      constraint
    else
      case constraint do
        %Range{min: ^version} = range -> %{range | include_min: true}
        %Range{max: ^version} = range -> %{range | include_max: true}
        _ -> Util.union([version, constraint])
      end
    end
  end

  def compare(%Version{} = left, %Version{} = right) do
    Version.compare(left, right)
  end

  def compare(%Version{} = version, %Range{min: min, include_min: include_min}) do
    if min == nil do
      :gt
    else
      case Version.compare(version, min) do
        :eq when include_min -> :eq
        :eq -> :lt
        :lt -> :lt
        :gt -> :gt
      end
    end
  end

  def compare(%Version{} = version, %Union{ranges: [range | _]}) do
    compare(version, range)
  end

  def min(left, right) do
    case compare(left, right) do
      :lt -> left
      :eq -> left
      :gt -> right
    end
  end

  def max(left, right) do
    case compare(left, right) do
      :lt -> right
      :eq -> left
      :gt -> left
    end
  end

  def to_range(%Version{} = version) do
    %Range{min: version, max: version, include_min: true, include_max: true}
  end

  def to_range(%Range{} = range) do
    range
  end

  def prioritize(%Version{} = left, %Version{} = right) do
    do_prioritize(left, right) == :gt
  end

  defp do_prioritize(left, right) do
    cond do
      left.pre != [] and right.pre == [] -> :lt
      left.pre == [] and right.pre != [] -> :gt
      true -> Version.compare(left, right)
    end
  end
end
