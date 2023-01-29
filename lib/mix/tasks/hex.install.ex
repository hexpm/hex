defmodule Mix.Tasks.Hex.Install do
  use Mix.Task

  @shortdoc false
  @moduledoc false
  @behaviour Hex.Mix.TaskDescription

  @impl true
  def run(args) do
    case args do
      [_] ->
        Mix.Tasks.Local.Hex.run(args)

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.install VERSION
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"VERSION", "Manually install specific Hex version"}
    ]
  end
end
