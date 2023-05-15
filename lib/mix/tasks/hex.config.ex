defmodule Mix.Tasks.Hex.Config do
  use Mix.Task

  @shortdoc "Reads, updates or deletes local Hex config"

  @moduledoc """
  Reads, updates or deletes local Hex config.

  ## List config keys and values

      $ mix hex.config

  ## Get or delete config value for KEY

      $ mix hex.config KEY [--delete]

  ## Set config KEY to VALUE

      $ mix hex.config KEY VALUE

  ## Config keys

    * `api_key` - Your API key. If you are authenticated this config will override
      the API key used for your authenticated user. Can be also be overridden by
      setting the environment variable `HEX_API_KEY`
    * `api_url` - Hex API URL. Can be overridden by setting the environment
      variable `HEX_API_URL` (Default: `#{inspect(Hex.State.default_api_url())}`)
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
      environment variable `HTTP_PROXY`
    * `https_proxy` - HTTPS proxy server. Can be overridden by setting the
      environment variable `HTTPS_PROXY`
    * `no_proxy` - A comma separated list of hostnames that will not be proxied,
      asterisks can be used as wildcards. Can be overridden by setting the
      environment variable `no_proxy` or `NO_PROXY`
    * `http_concurrency` - Limits the number of concurrent HTTP requests in
      flight. Can be overridden by setting the environment variable
      `HEX_HTTP_CONCURRENCY` (Default: `8`)
    * `http_timeout` - Sets the timeout for HTTP requests in seconds. Can be
      overridden by setting the environment variable `HEX_HTTP_TIMEOUT`
    * `mirror_url` - Hex mirror URL. Can be overridden by setting the
      environment variable `HEX_TRUSTED_MIRROR_URL`
    * `trusted_mirror_url` - Hex mirror URL. Unlike `mirror_url`, this mirror is
      "trusted", secrets such as authentication information will be sent to the
      mirror. Can be overridden by setting the environment variable
      `HEX_TRUSTED_MIRROR_URL`
    * `cacerts_path` - Path to the CA certificate store PEM file. If not set,
      a CA bundle that ships with Hex is used. Can be overridden by setting the
      environment variable `HEX_CACERTS_PATH`
    * `no_short_urls` - If set to true Hex will not
      shorten any links. Can be overridden by setting the environment variable
      `HEX_NO_SHORT_URLS` (Default: `false`)

  Hex responds to these additional environment variables:

    * `HEX_HOME` - directory where Hex stores the cache and configuration
      (Default: `~/.hex`)

    * `MIX_XDG` - asks Hex to follow the [XDG Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
      for its home directory and configuration files. `HEX_HOME` has higher preference
      than `MIX_XDG`. If none of the variables are set, the default directory
      `~/.hex` will be used.

  ## Config overrides

  All keys from the "Config keys" section above can be overridden.

  Hex uses the following order of precedence when computing a value for a given key:

    1. System environment

       Setting for example `HEX_API_URL` environment variable has always the
       highest precedence for the `api_url` config key.

    2. Project configuration

       Hex allows an optional, per-project configuration in the `mix.exs` file.

       For example, to override `api_url` config key, add the following:

           # mix.exs
           defmodule MyApp.MixProject
             def project() do
               [
                 # ...
                 deps: deps(),
                 hex: hex()
               ]
             end

             defp hex() do
               [
                 api_url: "https://hex.myorg/api"
               ]
             end
           end

    3. Global configuration using `mix hex.config KEY VALUE`

    4. Default value

  ## Command line options

    * `--delete` - Remove a specific config key
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [delete: :boolean]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = OptionParser.parse!(args, strict: @switches)

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

  @impl true
  def tasks() do
    [
      {"", "Reads, updates or deletes local Hex config"},
      {"KEY", "Get config value for KEY"},
      {"KEY --delete", "Delete config value for KEY"},
      {"KEY VALUE", "Set config KEY to VALUE"}
    ]
  end

  defp list() do
    Enum.each(valid_read_keys(), fn {config, _internal} ->
      read(config, true)
    end)
  end

  defp read(key, verbose \\ false)

  defp read(key, verbose) when is_binary(key) do
    key = String.to_atom(key)

    case Keyword.fetch(valid_read_keys(), key) do
      {:ok, internal} ->
        fetch_current_value_and_print(internal, key, verbose)

      _error ->
        Mix.raise("The key #{key} is not valid")
    end
  end

  defp read(key, verbose) when is_atom(key), do: read(to_string(key), verbose)

  defp fetch_current_value_and_print(internal, key, verbose) do
    case Map.fetch(Hex.State.get_all(), internal) do
      {:ok, {{:env, env_var}, value}} ->
        print_value(key, value, verbose, "(using `#{env_var}`)")

      {:ok, {{:global_config, _key}, value}} ->
        print_value(key, value, verbose, "(using `#{config_path()}`)")

      {:ok, {{:project_config, _key}, value}} ->
        print_value(key, value, verbose, "(using `mix.exs`)")

      {:ok, {kind, value}} when kind in [:default, :computed] ->
        print_value(key, value, verbose, "(default)")

      :error ->
        Mix.raise("Config does not contain the key #{key}")
    end
  end

  defp print_value(key, value, true, source),
    do: Hex.Shell.info("#{key}: #{inspect(value, pretty: true)} #{source}")

  defp print_value(_key, value, false, _source), do: Hex.Shell.info(inspect(value, pretty: true))

  defp delete(key) do
    key = String.to_atom(key)

    if Keyword.has_key?(valid_write_keys(), key) do
      Hex.Config.remove([key])
    end
  end

  defp set(key, value) do
    key = String.to_atom(key)

    if Keyword.has_key?(valid_write_keys(), key) do
      Hex.Config.update([{key, value}])
    else
      Mix.raise("Invalid key #{key}")
    end
  end

  defp config_path() do
    :config_home
    |> Hex.State.fetch!()
    |> Path.join("hex.config")
  end

  defp valid_keys() do
    Enum.map(Hex.State.config(), fn {internal, map} ->
      [config | _] = Map.get(map, :config, [nil])
      [env | _] = Map.get(map, :env, [nil])

      cond do
        String.starts_with?(to_string(config), "$") -> {internal, config, :not_accessible}
        is_nil(config) and not is_nil(env) -> {internal, config, :env_only}
        is_nil(config) and is_nil(env) -> {internal, config, :read_only}
        true -> {internal, config, :read_and_write}
      end
    end)
  end

  defp valid_read_keys() do
    valid_keys()
    |> Enum.map(fn {internal, config, access} ->
      if access != :not_accessible, do: key_representation(internal, config)
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp valid_write_keys() do
    valid_keys()
    |> Enum.map(fn {internal, config, access} ->
      if access == :read_and_write, do: key_representation(internal, config)
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp key_representation(internal, nil), do: {internal, internal}
  defp key_representation(internal, config), do: {config, internal}
end
