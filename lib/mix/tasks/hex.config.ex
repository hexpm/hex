defmodule Mix.Tasks.Hex.Config do
  use Mix.Task

  @shortdoc "Reads, updates or deletes local Hex config"

  @moduledoc """
  ## List config keys and values

      mix hex.config

  ## Get or delete config value for KEY

      mix hex.config KEY [--delete]

  ## Set config KEY to VALUE

      mix hex.config KEY VALUE

  ## Config keys

    * `api_key` - Your API key. If you are authenticated this config will override
      the API key used for your authenticated user. Can be also be overridden by
      setting the environment variable `HEX_API_KEY`
    * `api_url` - Hex API URL. Can be overridden by setting the environment
      variable `HEX_API_URL` (Default: `"https://hex.pm/api"`)
    * `offline` - If set to true Hex will not fetch the registry or packages and
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
    * `no_verify_repo_origin` - If set to true Hex will not verify the registry
      origin. Can be overridden by setting the environment variable
      `HEX_NO_VERIFY_REPO_ORIGIN` (Default: `false`)
    * `http_proxy` - HTTP proxy server. Can be overridden by setting the
      environment variable `HTTP_PROXY` (Default: `nil`)
    * `https_proxy` - HTTPS proxy server. Can be overridden by setting the
      environment variable `HTTPS_PROXY` (Default: `nil`)
    * `http_concurrency` - Limits the number of concurrent HTTP requests in
      flight. Can be overridden by setting the environment variable
      `HEX_HTTP_CONCURRENCY` (Default: `8`)
    * `http_timeout` - Sets the timeout for HTTP requests in seconds. Can be
      overridden by setting the environment variable `HEX_HTTP_TIMEOUT`
      (Default: `nil`)

  `HEX_HOME` environment variable can be set to point to the directory where Hex
  stores the cache and configuration (Default: `~/.hex`)

  ## Command line options

    * `--delete` - Remove a specific config key
  """

  @switches [delete: :boolean]
  @valid_write_keys [
    "api_url",
    "offline",
    "unsafe_https",
    "unsafe_registry",
    "no_verify_repo_origin",
    "http_proxy",
    "https_proxy",
    "http_concurrency",
    "http_timeout"
  ]

  @valid_read_keys [
    "api_url",
    "api_key_write_unencrypted",
    "offline",
    "unsafe_https",
    "unsafe_registry",
    "no_verify_repo_origin",
    "http_proxy",
    "https_proxy",
    "http_concurrency",
    "http_timeout",
    "home"
  ]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)

    case args do
      [] ->
        list()

      ["$" <> _key | _] ->
        Mix.raise("Invalid key name")

      [key] ->
        if opts[:delete] do
          delete(key)
        else
          read(key)
        end

      [key, value] ->
        set(key, value)

      _ ->
        Mix.raise("""
        Invalid arguments, expected:

        mix hex.config KEY [VALUE]
        """)
    end
  end

  defp list() do
    Enum.each(@valid_read_keys, fn key ->
      read(key, true)
    end)
  end

  defp read(key, verbose \\ false)

  defp read(key, verbose) when is_binary(key) and key in @valid_read_keys do
    case Map.fetch!(Hex.State.get_all(), :"#{key}") do
      {{:env, env_var}, value} ->
        print_value(key, value, verbose, "(using `#{env_var}`)")

      {{:global_config, _key}, value} ->
        print_value(key, value, verbose, "(using `#{config_path()}`)")

      {{:project_config, _key}, value} ->
        print_value(key, value, verbose, "(using `mix.exs`)")

      {kind, value} when kind in [:default, :computed] ->
        print_value(key, value, verbose, "(default)")
    end
  end

  defp read(key, verbose) when is_atom(key), do: read(to_string(key), verbose)
  defp read(key, _verbose), do: Mix.raise("The key #{key} is not valid")

  defp print_value(key, value, true, source),
    do: Hex.Shell.info("#{label(key)}: #{inspect(value, pretty: true)} #{source}")

  defp print_value(_key, value, false, _source), do: Hex.Shell.info(inspect(value, pretty: true))

  defp delete(key) do
    Hex.Config.remove([String.to_atom(key)])
  end

  defp set(key, value) when key in @valid_write_keys do
    Hex.Config.update([{:"#{key}", value}])
  end

  defp set(key, _value) do
    Mix.raise("Invalid key #{key}")
  end

  defp config_path() do
    :home
    |> Hex.State.fetch!()
    |> Path.join("hex.config")
  end

  defp label(key) do
    case key do
      "api_key_write_unencrypted" -> "api_key"
      _ -> key
    end
  end
end
