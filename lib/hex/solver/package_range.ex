# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.PackageRange do
  @moduledoc false

  alias Hex.Solver.PackageRange
  alias Hex.Solver.Constraints.Range

  defstruct repo: nil,
            name: nil,
            constraint: nil

  def to_string(%PackageRange{name: "$root"}), do: "your app"
  def to_string(%PackageRange{name: "$lock"}), do: "the lock"

  def to_string(%PackageRange{repo: nil, name: name, constraint: constraint}),
    do: "#{name}#{constraint(constraint)}"

  def to_string(%PackageRange{repo: repo, name: name, constraint: constraint}),
    do: "#{repo}/#{name}#{constraint(constraint)}"

  defp constraint(%Range{min: nil, max: nil}), do: ""
  defp constraint(constraint), do: " #{constraint}"

  defimpl String.Chars do
    defdelegate to_string(package_range), to: Hex.Solver.PackageRange
  end

  defimpl Inspect do
    def inspect(%{repo: nil, name: name, constraint: constraint}, _opts),
      do: "#PackageRange<#{name} #{constraint}>"

    def inspect(%{repo: repo, name: name, constraint: constraint}, _opts),
      do: "#PackageRange<#{repo}/#{name} #{constraint}>"
  end
end
