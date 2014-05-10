defmodule Mix.Tasks.Hex.Update do
  use Mix.Task

  @shortdoc "Update the hex registry file"

  @moduledoc """
  Update the hex registry file.

  `mix hex.update`

  Force fetches the registry file and bypasses any caching. This task does not
  have to be called unless there are issues with the registry file as hex
  fetches it automatically when needed.
  """

  def run(_args) do
    Hex.Util.update_registry(no_cache: true)
  end
end
