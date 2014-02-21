defmodule Mix.Tasks.Explex.Update do
  use Mix.Task

  def run(_args) do
    Explex.start_api
    Mix.shell.info("Downloading registry...")
    Explex.API.get_registry(Explex.Registry.path)
    Mix.shell.info("Updating registry was successful!")
  end
end
