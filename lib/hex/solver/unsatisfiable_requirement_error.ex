# Vendored from hex_solver v0.3.0 (c4f8f89), do not edit manually

defmodule Hex.Solver.UnsatisfiableRequirementError do
  _ = """
  Raised when a version requirement is valid but no version can satisfy it
  because two of its intersected ranges are disjoint.
  """

  defexception [:requirement, :left, :right]

  @impl true
  def message(%__MODULE__{requirement: requirement, left: left, right: right}) do
    "requirement #{inspect(requirement)} is unsatisfiable because " <>
      "#{inspect(left)} and #{inspect(right)} are disjoint"
  end
end
