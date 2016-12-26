defmodule Mix.Tasks.Hex.Config do
  use Mix.Task

  @shortdoc "Reads, updates or deletes Hex config"

  @moduledoc """
  Reads, updates or deletes Hex configuration keys.

      mix hex.config KEY [VALUE]

  ## Config keys

    * `username` - Hex username
    * `key` - Hex API key
    * `api_url` - Hex API URL. Can be overridden by setting the environment
      variable `HEX_API_URL` (Default: `"https://hex.pm/api"`)
    * `repo_url` - Hex repository URL. Can be overridden by setting the
      environment variable `HEX_REPO_URL` (Default: `nil`)
    * `mirror_url` - Hex.pm repository mirror URL. Use this option when using a
      Hex.pm mirror, unlike when setting `repo_url` this will still use the
      public key from Hex.pm when verifying the registry. Can be overridden by
      setting the environment variable `HEX_MIRROR_URL`. (Default: `nil`)
    * `offline` - If set to true Hex will not fetch the regsitry or packages and
      will instead use locally cached files if they are available. Can be
      overridden by setting the environment variable `HEX_OFFLINE` (Default:
      `false`)
    * `unsafe_https` - If set to true Hex will not verify HTTPS certificates.
      Can be overridden by setting the environment variable `HEX_UNSAFE_HTTPS`
      (Default: `false`)
    * `unsafe_registry` - If set to true Hex will not verify the registry
      signature against the repository's public key. Can be overridden by
      setting the environment variable `HEX_UNSAFE_REGISTRY` (Default:
      `false`)
    * `http_proxy` - HTTP proxy server. Can be overridden by setting the
      environment variable `HTTP_PROXY` (Default: `nil`)
    * `https_proxy` - HTTPS proxy server. Can be overridden by setting the
      environment variable `HTTPS_PROXY` (Default: `nil`)
    * `http_concurrency` - Limits the number of concurrent HTTP requests in
      flight. Can be overridden by setting the environment variable
      `HEX_HTTP_CONCURRENCY` (Default: `8`)

  ## Command line options

    * `--delete` - Remove a specific config key
  """

  @switches [delete: :boolean]

  def run(args) do
    Hex.start
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      [] ->
        list()
      ["$" <> _key | _] ->
        Mix.raise "Invalid key name"
      [key] ->
        if opts[:delete] do
          delete(key)
        else
          read(key)
        end
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

  defp read(key) do
    case Keyword.fetch(Hex.Config.read, :"#{key}") do
      {:ok, value} -> Hex.Shell.info inspect(value, pretty: true)
      :error       -> Mix.raise "Config does not contain key #{key}"
    end
  end

  defp delete(key) do
    Hex.Config.remove([String.to_atom(key)])
  end

  defp set(key, value) do
    Hex.Config.update([{:"#{key}", value}])
  end
end
