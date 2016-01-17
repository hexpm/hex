defmodule Mix.Tasks.Hex do
  use Mix.Task

  @shortdoc "Prints Hex help information"

  @moduledoc """
  Prints Hex tasks and their information.

  `mix hex`

  Hex and some Mix tasks can be configured with the following environment
  variables:

    * `HEX_HOME` - Sets the directory where Hex stores the cache and
      configuration (Default: `~/.hex`)
    * `HEX_API` - Sets the API URL (Default: `https://hex.pm/api`)
    * `HEX_CDN` - Sets the CDN URL (Default: `https://s3.amazonaws.com/s3.hex.pm`)
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    Hex.start

    case args do
      [] -> general()
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex"
    end
  end

  defp general() do
    Hex.Shell.info "Hex v" <> Hex.version
    Hex.Shell.info "Hex is a package manager for the Erlang ecosystem."
    line_break()

    if Hex.Version.match?(System.version, ">= 1.1.0-dev") do
      Hex.Shell.info "Available tasks:"
      line_break()
      Mix.Task.run("help", ["--search", "hex."])
      line_break()
    end

    Hex.Shell.info "Further information can be found here: https://hex.pm/docs/tasks"
  end

  def line_break(), do: Hex.Shell.info ""
end
