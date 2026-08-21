# Vendored from hex_solver v0.2.3 (291c624), do not edit manually

defmodule Hex.Solver.Constraints.Impl do
  @moduledoc false

  defmacro __using__(opts) do
    for = Keyword.get(opts, :for, __CALLER__.module)

    quote do
      defimpl Hex.Solver.Constraint, for: unquote(for) do
        def any?(constraint),
          do: unquote(__CALLER__.module).any?(constraint)

        def empty?(constraint),
          do: unquote(__CALLER__.module).empty?(constraint)

        def allows?(constraint, version),
          do: unquote(__CALLER__.module).allows?(constraint, version)

        def allows_any?(left, right),
          do: unquote(__CALLER__.module).allows_any?(left, right)

        def allows_all?(left, right),
          do: unquote(__CALLER__.module).allows_all?(left, right)

        def difference(left, right),
          do: unquote(__CALLER__.module).difference(left, right)

        def intersect(left, right),
          do: unquote(__CALLER__.module).intersect(left, right)

        def union(left, right),
          do: unquote(__CALLER__.module).union(left, right)

        def compare(left, right),
          do: unquote(__CALLER__.module).compare(left, right)

        def to_requirement(constraint),
          do: unquote(__CALLER__.module).to_requirement(constraint)
      end
    end
  end
end
