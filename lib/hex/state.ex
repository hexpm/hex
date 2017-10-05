defmodule Hex.State do
  @name __MODULE__
  @api_url "https://hex.pm/api"
  @logged_keys ~w(http_proxy HTTP_PROXY https_proxy HTTPS_PROXY)
  @default_home "~/.hex"
  @pbkdf2_iters 32_768

  def start_link() do
    config = Hex.Config.read()
    Agent.start_link(__MODULE__, :init, [config], [name: @name])
  end

  def stop() do
    Agent.stop(@name)
  end


  def init(config) do
    %{
      home: System.get_env("HEX_HOME") |> default(@default_home) |> Path.expand(),
      repos: Hex.Config.read_repos(config),
      api_url: load_config(config, ["HEX_API_URL", "HEX_API"], :api_url) |> trim_slash() |> default(@api_url),
      api_key: load_config(config, [], :encrypted_key),
      mirror_url: load_config(config, ["HEX_MIRROR_URL", "HEX_MIRROR"], :mirror_url) |> trim_slash(),
      http_proxy: load_config(config, ["http_proxy", "HTTP_PROXY"], :http_proxy),
      https_proxy: load_config(config, ["https_proxy", "HTTPS_PROXY"], :https_proxy),
      offline?: load_config(config, ["HEX_OFFLINE"], :offline) |> to_boolean() |> default(false),
      check_cert?: load_config(config, ["HEX_UNSAFE_HTTPS"], :unsafe_https) |> to_boolean() |> default(false) |> Kernel.not(),
      check_registry?: load_config(config, ["HEX_UNSAFE_REGISTRY"], :unsafe_registry) |> to_boolean() |> default(false) |> Kernel.not(),
      http_concurrency: load_config(config, ["HEX_HTTP_CONCURRENCY"], :http_concurrency) |> to_integer() |> default(8),
      http_timeout: load_config(config, ["HEX_HTTP_TIMEOUT"], :http_timeout) |> to_integer() |> http_timeout(),
      httpc_profile: :hex,
      ssl_version: ssl_version(),
      pbkdf2_iters: @pbkdf2_iters,
      clean_pass: true,
    }
  end

  def refresh() do
    Agent.update(@name, fn _ ->
      init(Hex.Config.read())
    end)
  end

  def fetch(key) do
    Agent.get(@name, Map, :fetch, [key])
  end

  def fetch!(key) do
    case fetch(key) do
      {:ok, value} ->
        value
      :error ->
        raise KeyError, key: key, term: Hex.State
    end
  end

  def get(key, default \\ nil) do
    case fetch(key) do
      {:ok, value} -> value
      :error -> default
    end
  end

  def put(key, value) do
    Agent.update(@name, Map, :put, [key, value])
  end

  def update!(key, fun) do
    Agent.update(@name, fn state ->
      Map.update!(state, key, fun)
    end)
  end

  def get_all() do
    Agent.get(@name, & &1)
  end

  def put_all(map) do
    Agent.update(@name, fn _ -> map end)
  end

  defp load_config(config, envs, config_key) do
    result =
      envs
      |> Enum.map(&env_exists/1)
      |> Enum.find(&(not is_nil &1))
      || config_exists(config, config_key)

    if result do
      {key, value} = result
      log_value(key, value)
      value
    end
  end

  defp env_exists(key) do
    if value = System.get_env(key) do
      {key, value}
    else
      nil
    end
  end

  defp config_exists(config, key) do
    if value = Keyword.get(config, key) do
      {"config[:#{key}]", value}
    else
      nil
    end
  end

  defp log_value(key, value) do
    if function_exported?(Mix, :debug?, 0) and Mix.debug? do
      if key in @logged_keys do
        Hex.Shell.info "Using #{key} = #{value}"
      end
    end
  end

  defp to_boolean(nil),     do: nil
  defp to_boolean(false),   do: false
  defp to_boolean(true),    do: true
  defp to_boolean("0"),     do: false
  defp to_boolean("1"),     do: true
  defp to_boolean("false"), do: false
  defp to_boolean("true"),  do: true

  defp to_integer(nil), do: nil
  defp to_integer(""), do: nil
  defp to_integer(string) do
    {int, _} = Integer.parse(string)
    int
  end

  defp default(nil, value), do: value
  defp default(value, _),   do: value

  defp trim_slash(nil), do: nil
  defp trim_slash(string) do
    if String.ends_with?(string, "/") do
      string
      |> :binary.part(0, byte_size(string) - 1)
      |> trim_slash()
    else
      string
    end
  end

  defp ssl_version() do
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

  defp http_timeout(nil), do: nil
  defp http_timeout(seconds), do: seconds * 1000
end
