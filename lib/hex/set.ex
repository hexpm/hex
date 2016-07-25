defmodule Hex.Set do
  if Version.compare(System.version, "1.1.0") == :lt do
    @module HashSet
  else
    @module MapSet
  end

  defdelegate new(), to: @module
  defdelegate put(set, value), to: @module
end
