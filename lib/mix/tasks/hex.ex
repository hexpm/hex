defmodule Mix.Tasks.Hex do
  use Mix.Task

  def run(args) do
    Mix.Task.run("hex.info", args)
  end
end
