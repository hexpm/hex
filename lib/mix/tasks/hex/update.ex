defmodule Mix.Tasks.Hex.Update do
  use Mix.Task

  @shortdoc "Update hex registry file"

  @moduledoc """
  Update the hex registry file. This should be done periodically. The registry
  status can be checked with `mix hex.info`.

  The registry contains an index of all packages, releases and their
  dependencies that is used during hex's dependency resolution.

  `mix hex.update`
  """

  @aliases [q: :quiet]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)

    Hex.start_api
    unless opts[:quiet], do: Mix.shell.info("Downloading registry...")
    Hex.API.get_registry(Hex.Registry.path)
    unless opts[:quiet], do: Mix.shell.info("Updating registry was successful!")
  end
end
