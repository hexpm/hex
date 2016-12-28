defmodule Mix.Tasks.Hex.Config do
  use Mix.Task

  @shortdoc "Reads, updates or deletes Hex config"

  @moduledoc """
  Reads, updates or deletes Hex configuration keys.

      mix hex.config KEY [VALUE]

  ## Config keys

    * `username` - Hex username
    * `key` - Hex API key
    * `api_url` - Hex API URL, can be overridden by setting
      the environment variable `HEX_API`
    * `repo_url` - Hex repository URL, can be overridden by setting
      the environment variable `HEX_REPO`
    * `mirror_url` - Hex.pm repository mirror URL, can be overridden by setting
      the environment variable `HEX_MIRROR`.
      Use this option when using a Hex.pm mirror, unlike when setting `repo_url`
      this will still use the public key from Hex.pm when verifying the registry
    * `offline` - If set to true Hex will not fetch the regsitry or packages and
      will instead the locally cached files if they are available
    * `unsafe_https` - If set to true Hex will not verify HTTPS certificates
    * `unsafe_registry` - If set to true Hex will not verify the registry
      signature against the repository's public key
    * `http_proxy` - HTTP proxy server
    * `https_proxy` - HTTPS proxy server
    * `http_concurrency` - Limits the number of concurrent HTTP requests in
      flight, can be overridden by setting `HEX_HTTP_CONCURRENCY`

  ## Command line options

    * `--delete` - Remove a specific config key

  ## Env variabies
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
  """

  @switches [delete: :boolean]

  def run(args) do
    Hex.start
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      [] ->
        list()
      [key] ->
        read(key, !!opts[:delete])
      [key, value] ->
        set(key, value)
      _ ->
        Mix.raise """
        Invalid arguments, expected:
        mix hex.config KEY [VALUE]
        """
    end
  end

  defp list do
    Enum.each(Hex.Config.read, fn {key, value} ->
      Hex.Shell.info "#{key}: #{inspect(value, pretty: true)}"
    end)
  end

  def read(key, true) do
    Hex.Config.remove([String.to_atom(key)])
    Hex.Shell.info "Deleted config #{key}"
  end
  def read(key, false) do
    case Keyword.fetch(Hex.Config.read, :"#{key}") do
      {:ok, value} -> Hex.Shell.info inspect(value, pretty: true)
      :error       -> Mix.raise "Config does not contain a key #{key}"
    end
  end

  def set(key, value) do
    Hex.Config.update([{:"#{key}", value}])
    Hex.Shell.info "#{key}: #{inspect(value, pretty: true)}"
  end
end
