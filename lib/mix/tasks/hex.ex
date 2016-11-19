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
    * `HEX_REPO` - Sets the repository URL (not set by default)
    * `HEX_MIRROR` - Sets the Hex.pm mirror URL (Default: `https://repo.hex.pm`).
      Use this option when using a Hex.pm mirror, unlike when setting `HEX_REPO`
      this will still use the public key from Hex.pm when verifying the registry
    * `HEX_OFFLINE` - If set Hex will not fetch the registry or packages and will
      instead the locally cached files if they are available
    * `HEX_UNSAFE_HTTPS` - If set Hex will not verify HTTPS certificates
    * `HEX_UNSAFE_REGISTRY` - If set Hex will not verify the registry signature
      against the repository's public key
    * `HTTP_PROXY` / `HTTPS_PROXY` - Sets the URL to a HTTP(S) proxy, the
      environment variables can also be in lower case
    * `HEX_HTTP_CONCURRENCY` - Limits the number of concurrent HTTP requests in
      flight, (Default: 8)
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
