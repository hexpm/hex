defmodule Mix.Tasks.Explex.Update do
  use Mix.Task

  def run(_args) do
    path = Path.join(Mix.Utils.mix_home, "registry.dets")
    Mix.shell.info("Downloading registry...")
    Explex.API.get_registry(path)
    Mix.shell.info("Updating registry was successful!")
  end
end
