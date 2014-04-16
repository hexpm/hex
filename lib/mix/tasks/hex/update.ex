defmodule Mix.Tasks.Hex.Update do
  use Mix.Task

  @install_url "http://hex.pm/installs/hex.ez"

  @shortdoc "Update hex"

  @moduledoc """
  Update the hex installation.

  `mix hex.update`

  ## Command line options

  * `--channel`, `-c` - Selects the channel, defaults to the currently installed
    channel (possible values: `stable` `dev`)
  """

  @aliases [c: :channel]

  def run(args) do
    { opts, _, _ } = OptionParser.parse(args, aliases: @aliases)

    channel = URI.encode channel(opts)
    elixir  = URI.encode System.version
    url = @install_url <> "?channel=#{channel}&elixir=#{elixir}"

    Mix.shell.info("Updating Hex installation...")
    Mix.Task.run "local.install", [url, "--force"]
  end

  defp channel(opts) do
    opts[:channel] || atom_to_binary(Hex.channel)
  end
end
