defmodule Hex.Set do
  if Version.compare(System.version, "1.1.0") == :lt do
    @module HashSet
  else
    @module MapSet
  end
  
  def new(enum) do
    Enum.into(enum, new())
  end

  defdelegate new(), to: @module
  defdelegate put(set, value), to: @module
  defdelegate delete(set, value), to: @module
end
