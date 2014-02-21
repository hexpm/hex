defmodule Mix.Tasks.Hex.Update do
  use Mix.Task

  def run(_args) do
    Hex.start_api
    Mix.shell.info("Downloading registry...")
    Hex.API.get_registry(Hex.Registry.path)
    Mix.shell.info("Updating registry was successful!")
  end
end
