defmodule Hex.Plugin do
  @callback init :: any

  def init do
    :ok = Hex.Registry.append({:hex, Hex.Registry.ETS, Hex.SCM})
    Mix.SCM.append(Hex.SCM)
  end
end
