defmodule Hex.State do
  @name __MODULE__
  @logged_keys ~w(http_proxy HTTP_PROXY https_proxy HTTPS_PROXY)

  def start_link do
    config = Hex.Config.read
    Agent.start_link(__MODULE__, :init, [config], [name: @name])
  end

  def init(config) do
    %{home: Path.expand(System.get_env("HEX_HOME") || "~/.hex"),
      api: load_config(config, ["HEX_API"], :api_url) || "https://hex.pm",
      cdn: load_config(config, ["HEX_CDN"], :cdn_url) || "https://s3.amazonaws.com/s3.hex.pm",
      registry_updated: false}
  end

  def fetch(key) do
    Agent.get(@name, Map, :fetch, [key])
  end

  def fetch!(key) do
    Agent.get(@name, Map, :fetch!, [key])
  end

  def get(key, default \\ nil) do
    Agent.get(@name, Map, :get, [key, default])
  end

  def put(key, value) do
    Agent.update(@name, Map, :put, [key, value])
  end

  def load_config(config, envs, config_key) do
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
    if Enum.member?(@logged_keys, key) do
      Hex.Shell.info "Using #{key} = #{value}"
    end
  end
end
