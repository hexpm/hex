# Vendored from hex_solver v0.2.3 (057f77e), do not edit manually

defmodule Hex.Solver.Util do
  @moduledoc false

  def compare(module) when is_atom(module) do
    &(module.compare(&1, &2) in [:lt, :eq])
  end
end
