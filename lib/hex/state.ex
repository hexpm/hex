defmodule Hex.State do
  @name __MODULE__
  @logged_keys ~w(http_proxy HTTP_PROXY https_proxy HTTPS_PROXY)
  @default_home "~/.hex"
  @default_url "https://hex.pm/api"
  @default_mirror "https://repo.hex.pm"

  @hexpm_pk """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
  Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
  IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
  3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
  XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
  J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
  0wIDAQAB
  -----END PUBLIC KEY-----
  """

  def start_link do
    config = Hex.Config.read
    Agent.start_link(__MODULE__, :init, [config], [name: @name])
  end

  def stop do
    Agent.stop(@name)
  end

  def init(config) do
    cdn    = load_config(config, ["HEX_CDN"], :cdn_url)
    mirror = load_config(config, ["HEX_MIRROR"], :mirror_url)

    if cdn do
      Hex.Shell.warn cdn_message()
    end

    %{home:             System.get_env("HEX_HOME") |> default(@default_home) |> Path.expand,
      api:              load_config(config, ["HEX_API"], :api_url) |> default(@default_url) |> trim_slash,
      repo:             load_config(config, ["HEX_REPO"], :repo_url) |> trim_slash,
      mirror:           default(mirror || cdn, @default_mirror) |> trim_slash,
      http_proxy:       load_config(config, ["http_proxy", "HTTP_PROXY"], :http_proxy),
      https_proxy:      load_config(config, ["https_proxy", "HTTPS_PROXY"], :https_proxy),
      offline?:         load_config(config, ["HEX_OFFLINE"], :offline) |> to_boolean |> default(false),
      check_cert?:      load_config(config, ["HEX_UNSAFE_HTTPS"], :unsafe_https) |> to_boolean |> default(false) |> Kernel.not,
      check_registry?:  load_config(config, ["HEX_UNSAFE_REGISTRY"], :unsafe_registry) |> to_boolean |> default(false) |> Kernel.not,
      hexpm_pk:         @hexpm_pk,
      httpc_profile:    :hex,
      ssl_version:      ssl_version(),
      pbkdf2_iters:     32768,
      clean_pass:       true}
  end

  # Work around for :socket_closed_remotely errors in httpc
  # Use a unique profile for each request to avoid races
  if Mix.env == :test do
    def fetch(:httpc_profile) do
      profile = make_ref() |> :erlang.ref_to_list |> List.to_atom
      {:ok, _pid} = :httpc_manager.start_link(profile, :only_session_cookies, :stand_alone)
    end
  end

  def fetch(key) do
    Agent.get(@name, Map, :fetch, [key])
  end

  def fetch!(key) do
    case fetch(key) do
      {:ok, value} -> value
      :error -> raise KeyError, key: key, term: Hex.State
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

  def get_all do
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

  defp default(nil, value), do: value
  defp default(value, _),   do: value

  defp trim_slash(nil), do: nil
  defp trim_slash(string) do
    if String.ends_with?(string, "/") do
      string
      |> :binary.part(0, byte_size(string) - 1)
      |> trim_slash
    else
      string
    end
  end

  defp cdn_message do
    "Hex.pm has moved to fastly and no longer uses the cdn " <>
    "configuration. " <> unset_message() <>
    "If you would still like to use a mirror or custom repository, read the docs at:\n\n" <>
    "    mix help hex.config\n"
  end

  defp unset_message do
    if System.get_env("HEX_CDN") do
      "To silence this warning you need to unset the environment variable HEX_CDN. "
    else
      "To silence this warning you need to run:\n\n    mix hex.config cdn_url --delete\n\n"
    end
  end

  defp ssl_version do
    {:ok, version} = :application.get_key(:ssl, :vsn)
    parse_ssl_version(version)
  end

  defp parse_ssl_version(version) do
    version
    |> List.to_string
    |> String.split(".")
    |> Enum.take(3)
    |> Enum.map(&to_integer/1)
    |> version_pad
    |> List.to_tuple
  end

  defp version_pad([major]),
    do: [major, 0, 0]
  defp version_pad([major, minor]),
    do: [major, minor, 0]
  defp version_pad([major, minor, patch]),
    do: [major, minor, patch]
  defp version_pad([major, minor, patch | _]),
    do: [major, minor, patch]

  defp to_integer(string) do
    {int, _} = Integer.parse(string)
    int
  end
end
