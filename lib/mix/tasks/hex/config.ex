defmodule Mix.Tasks.Hex.Config do
  use Mix.Task

  @shortdoc "Read or update hex config"

  @moduledoc """
  Reads or updates hex configuration file.

  `mix hex.config KEY [VALUE]`

 ## Config keys
  - `username` - Hex username
  - `key` - Hex API key
  - `api_url` - Hex API base URL (without trailing slash)
  - `cdn_url` - Hex CDN base URL (without trailing slash)
  - `http_proxy` - HTTP proxy server
  - `https_proxy` - HTTPS proxy server
  """

  def run(args) do
    Hex.start
    Hex.Util.ensure_registry(update: false)

    case args do
      [key] ->
        case Keyword.fetch(Hex.Config.read, :"#{key}") do
          {:ok, value} -> Hex.Shell.info inspect(value, pretty: true)
          :error       -> Mix.raise "Config does not contain a key #{key}"
        end
      [key, value] ->
        Hex.Config.update([{:"#{key}", value}])
        Hex.Shell.info "#{key}: #{inspect(value, pretty: true)}"
      _ ->
        Mix.raise "Invalid arguments, expected: mix hex.config KEY [VALUE]"
    end
  end
end
