defmodule Mix.Tasks.Hex.Key do
  use Mix.Task

  @moduledoc false

  def run(_) do
    Hex.start

    deprecation_msg = """
    [deprecation] The mix hex.key task is deprecated, please use:
      mix hex.user
    """
    Mix.raise deprecation_msg
  end
end
