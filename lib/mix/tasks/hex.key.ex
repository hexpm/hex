defmodule Mix.Tasks.Hex.Key do
  use Mix.Task

  @moduledoc false

  def run(_) do
    Mix.raise """
    [deprecation] The mix hex.key task is deprecated, please use:
      mix hex.user
    """
  end
end
