defmodule Hex.State do
  @moduledoc false

  @name __MODULE__
  @api_url "https://hex.pm/api"
  @pbkdf2_iters 32_768

  def default_api_url(), do: @api_url

  @config %{
    api_key_read: %{
      config: [:"$read_key"]
    },
    api_key_write: %{
      config: [:"$write_key", :"$encrypted_key"]
    },
    api_key_write_unencrypted: %{
      env: ["HEX_API_KEY"],
      config: [:api_key]
    },
    api_url: %{
      env: ["HEX_API_URL", "HEX_API"],
      config: [:api_url],
      default: @api_url,
      fun: {__MODULE__, :trim_slash}
    },
    cache_home: %{
      default: :user_cache,
      fun: {Hex.Config, :find_config_home}
    },
    config_home: %{
      default: :user_config,
      fun: {Hex.Config, :find_config_home}
    },
    unsafe_https: %{
      env: ["HEX_UNSAFE_HTTPS"],
      config: [:unsafe_https],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    unsafe_registry: %{
      env: ["HEX_UNSAFE_REGISTRY"],
      config: [:unsafe_registry],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    no_verify_repo_origin: %{
      env: ["HEX_NO_VERIFY_REPO_ORIGIN"],
      config: [:no_verify_repo_origin],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    http_concurrency: %{
      env: ["HEX_HTTP_CONCURRENCY"],
      config: [:http_concurrency],
      default: 8,
      fun: {__MODULE__, :to_integer}
    },
    http_proxy: %{
      env: ["http_proxy", "HTTP_PROXY"],
      config: [:http_proxy]
    },
    https_proxy: %{
      env: ["https_proxy", "HTTPS_PROXY"],
      config: [:https_proxy]
    },
    no_proxy: %{
      env: ["no_proxy", "NO_PROXY"],
      config: [:no_proxy]
    },
    http_timeout: %{
      env: ["HEX_HTTP_TIMEOUT"],
      config: [:http_timeout],
      fun: {__MODULE__, :to_integer}
    },
    data_home: %{
      default: :user_data,
      fun: {Hex.Config, :find_config_home}
    },
    mirror_url: %{
      env: ["HEX_MIRROR_URL", "HEX_MIRROR"],
      config: [:mirror_url],
      fun: {__MODULE__, :trim_slash}
    },
    trusted_mirror_url: %{
      env: ["HEX_TRUSTED_MIRROR_URL", "HEX_TRUSTED_MIRROR"],
      config: [:trusted_mirror_url],
      fun: {__MODULE__, :trim_slash}
    },
    offline: %{
      env: ["HEX_OFFLINE"],
      config: [:offline],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    resolve_verbose: %{
      env: ["HEX_RESOLVE_VERBOSE"],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    repos_key: %{
      env: ["HEX_REPOS_KEY"],
      config: [:repos_key]
    },
    diff_command: %{
      env: ["HEX_DIFF_COMMAND"],
      config: [:diff_command],
      default: Mix.Tasks.Hex.Package.default_diff_command()
    },
    cacerts_path: %{
      env: ["HEX_CACERTS_PATH"],
      default: nil,
      config: [:cacerts_path]
    },
    no_short_urls: %{
      env: ["HEX_NO_SHORT_URLS"],
      config: [:no_short_urls],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    debug_solver: %{
      env: ["HEX_DEBUG_SOLVER"],
      config: [:debug_solver],
      default: false,
      fun: {__MODULE__, :to_boolean}
    },
    ci: %{
      env: ["CI"],
      default: false,
      fun: {__MODULE__, :to_boolean}
    }
  }

  def start_link([]) do
    global_config = Hex.Config.read()
    Agent.start_link(__MODULE__, :init, [global_config], name: @name)
  end

  def child_spec(arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [arg]}
    }
  end

  def stop() do
    Agent.stop(@name)
  end

  def init(global_config) do
    project_config = Keyword.get(Mix.Project.config(), :hex, [])

    state =
      Map.new(@config, fn {key, spec} ->
        {key, load_config_value(global_config, project_config, spec)}
      end)

    Map.merge(state, %{
      clean_pass: {:computed, true},
      httpc_profile: {:computed, :hex},
      pbkdf2_iters: {:computed, @pbkdf2_iters},
      repos: {:computed, Hex.Config.read_repos(global_config)},
      ssl_version: {:computed, ssl_version()},
      shell_process: {:computed, nil}
    })
  end

  def refresh() do
    Agent.update(@name, fn _ ->
      init(Hex.Config.read())
    end)
  end

  def get(key) do
    Agent.get(@name, fn state ->
      case Map.fetch(state, key) do
        {:ok, {_source, value}} -> value
        :error -> nil
      end
    end)
  end

  def fetch!(key) do
    Agent.get(@name, fn state ->
      case Map.fetch(state, key) do
        {:ok, {_source, value}} ->
          value

        :error ->
          raise KeyError, key: key, term: Hex.State
      end
    end)
  end

  def fetch!(key, transform) do
    key
    |> fetch!()
    |> transform.()
  end

  def fetch_source!(key) do
    Agent.get(@name, fn state ->
      case Map.fetch(state, key) do
        {:ok, {source, _value}} ->
          source

        :error ->
          raise KeyError, key: key, term: Hex.State
      end
    end)
  end

  def put(key, value) do
    Agent.update(@name, Map, :put, [key, {:computed, value}])
  end

  def update!(key, fun) do
    Agent.update(@name, fn state ->
      Map.update!(state, key, fn {source, value} ->
        {source, fun.(value)}
      end)
    end)
  end

  def get_all() do
    Agent.get(@name, & &1)
  end

  def put_all(map) do
    Agent.update(@name, fn _ -> map end)
  end

  defp load_config_value(global_config, project_config, spec) do
    env = System.get_env()

    result =
      load_env(spec[:env], env) ||
        load_project_config(project_config, spec[:config]) ||
        load_global_config(global_config, spec[:config])

    {module, func} = spec[:fun] || {__MODULE__, :ok_wrap}

    case result do
      nil ->
        case apply(module, func, [spec[:default]]) do
          {:ok, value} ->
            {:default, value}

          {source, value} ->
            {source, value}
        end

      {source, value} ->
        case apply(module, func, [value]) do
          {:ok, value} ->
            {source, value}

          :error ->
            print_invalid_config_error(value, source)
            {:ok, value} = apply(module, func, [spec[:default]])
            {:default, value}
        end
    end
  end

  defp print_invalid_config_error(value, source) do
    value = inspect(value, pretty: true)
    message = "Invalid Hex config, falling back to default. Source: #{source(source)} #{value}"
    Hex.Shell.error(message)
  end

  defp source({:env, env_var}), do: "environment variable #{env_var}="
  defp source({:project_config, key}), do: "mix.exs config #{key}: "
  defp source({:global_config, key}), do: "Hex config (location: #{config_path()}) #{key}: "

  defp config_path() do
    :config_home
    |> Hex.State.fetch!()
    |> Path.join("hex.config")
  end

  defp load_env(keys, env) do
    Enum.find_value(keys || [], fn key ->
      case Map.fetch(env, key) do
        {:ok, value} -> {{:env, key}, value}
        :error -> nil
      end
    end)
  end

  defp load_global_config(config, keys) do
    Enum.find_value(keys || [], fn key ->
      if value = Keyword.get(config, key) do
        {{:global_config, key}, value}
      end
    end)
  end

  defp load_project_config(config, keys) do
    Enum.find_value(keys || [], fn key ->
      if value = Keyword.get(config, key) do
        {{:project_config, key}, value}
      end
    end)
  end

  def to_boolean(nil), do: {:ok, nil}
  def to_boolean(false), do: {:ok, false}
  def to_boolean(true), do: {:ok, true}
  def to_boolean("0"), do: {:ok, false}
  def to_boolean("1"), do: {:ok, true}
  def to_boolean("false"), do: {:ok, false}
  def to_boolean("true"), do: {:ok, true}
  def to_boolean("FALSE"), do: {:ok, false}
  def to_boolean("TRUE"), do: {:ok, true}
  def to_boolean(_), do: :error

  def to_integer(nil), do: {:ok, nil}
  def to_integer(""), do: {:ok, nil}
  def to_integer(integer) when is_integer(integer), do: {:ok, integer}

  def to_integer(string) when is_binary(string) do
    {int, _} = Integer.parse(string)
    {:ok, int}
  end

  def to_integer(_), do: :error

  def default(nil, value), do: value
  def default(value, _), do: value

  def trim_slash(nil), do: {:ok, nil}

  def trim_slash(string) when is_binary(string),
    do: {:ok, String.trim_leading(string, "/")}

  def trim_slash(_), do: :error

  def ssl_version() do
    {:ok, version} = :application.get_key(:ssl, :vsn)
    parse_ssl_version(version)
  end

  defp parse_ssl_version(version) do
    version
    |> List.to_string()
    |> String.split(".")
    |> Enum.take(3)
    |> Enum.map(&to_integer/1)
    |> version_pad()
    |> List.to_tuple()
  end

  defp version_pad([major]), do: [major, 0, 0]
  defp version_pad([major, minor]), do: [major, minor, 0]
  defp version_pad([major, minor, patch]), do: [major, minor, patch]
  defp version_pad([major, minor, patch | _]), do: [major, minor, patch]

  def path_expand(path) when is_binary(path) do
    {:ok, Path.expand(path)}
  end

  def path_expand(_), do: :error

  def ok_wrap(arg), do: {:ok, arg}

  def config, do: @config
end
