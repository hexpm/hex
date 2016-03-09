defmodule Mix.Tasks.Hex.Config do
  use Mix.Task

  @shortdoc "Reads or updates Hex config"

  @moduledoc """
  Reads or updates Hex configuration file.

      mix hex.config KEY [VALUE]

  ## Config keys

    * `username` - Hex username
    * `key` - Hex API key
    * `api_url` - Hex API URL (without trailing slash)
    * `repo_url` - Hex repository URL (without trailing slash)
    * `mirror_url` - Hex.pm repository mirror URL (without trailing slash).
      Use this option when using a Hex.pm mirror, unlike when setting `repo_url`
      this will still use the public key from Hex.pm when verifying the registry
    * `http_proxy` - HTTP proxy server
    * `https_proxy` - HTTPS proxy server

  ## Mirrors

  Hex provides 3 different mirrors for the following areas:

    * Americas (default) - https://s3.amazonaws.com/s3.hex.pm
    * Europe, Africa - https://s3-eu-west-1.amazonaws.com/s3-eu.hex.pm
    * Asia, Oceania - https://s3-ap-southeast-1.amazonaws.com/s3-asia.hex.pm

  You can change the default mirror as follows:

      mix hex.config mirror_url https://s3-eu-west-1.amazonaws.com/s3-eu.hex.pm
  """

  @switches [delete: :boolean]

  def run(args) do
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    Hex.start
    Hex.Utils.ensure_registry(update: false)

    case args do
      [] ->
        list()
      [key] ->
        read(key, !!opts[:delete])
      [key, value] ->
        set(key, value)
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.config KEY [VALUE]"
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
