# Vendored from hex_solver v0.1.0 (d6269a8), do not edit manually

defprotocol Hex.Solver.Constraint do
  @moduledoc false

  def any?(constraint)
  def empty?(constraint)
  def allows?(constraint, version)
  def allows_any?(left, right)
  def allows_all?(left, right)
  def difference(left, right)
  def intersect(left, right)
  def union(left, right)
  def compare(left, right)
end
