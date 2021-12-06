# Vendored from hex_solver v0.1.0 (fa05cce), do not edit manually

defmodule Hex.Solver.PackageRange do
  @moduledoc false

  alias Hex.Solver.PackageRange
  alias Hex.Solver.Constraints.Range

  defstruct name: nil,
            constraint: nil

  def to_string(%PackageRange{name: "$root"}), do: "your app"
  def to_string(%PackageRange{name: "$lock"}), do: "lock"

  def to_string(%PackageRange{name: name, constraint: constraint}),
    do: "#{name}#{constraint(constraint)}"

  defp constraint(%Range{min: nil, max: nil}), do: ""
  defp constraint(constraint), do: " #{constraint}"

  defimpl String.Chars do
    defdelegate to_string(package_range), to: Hex.Solver.PackageRange
  end

  defimpl Inspect do
    def inspect(%{name: name, constraint: constraint}, _opts) do
      "#PackageRange<#{name} #{constraint}>"
    end
  end
end
