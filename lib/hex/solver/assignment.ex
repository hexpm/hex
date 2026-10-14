# Vendored from hex_solver v0.3.1 (2a24512), do not edit manually

defmodule Hex.Solver.Assignment do
  @moduledoc false

  alias Hex.Solver.{Assignment, Incompatibility, Term}

  defstruct term: nil,
            decision_level: nil,
            index: nil,
            cause: nil

  def intersect(%Assignment{} = left, %Assignment{} = right) do
    %{left | term: Term.intersect(left.term, right.term)}
  end

  def decision?(%Assignment{cause: cause}) do
    cause == nil
  end

  def to_string(%Assignment{term: term}) do
    Term.to_string(term)
  end

  defimpl String.Chars do
    defdelegate to_string(assignment), to: Hex.Solver.Assignment
  end

  defimpl Inspect do
    def inspect(
          %{
            term: term,
            decision_level: decision_level,
            index: index,
            cause: cause
          },
          _opts
        ) do
      "#Assignment<term: #{Term.to_string(term)}#{maybe(", cause: ", cause)}, level: #{decision_level}, index: #{index}>"
    end

    defp maybe(_prefix, nil), do: ""
    defp maybe(prefix, value), do: "#{prefix}#{Incompatibility.to_string(value)}"
  end
end
