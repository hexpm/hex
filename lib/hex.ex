defmodule Hex do
  @default_url "https://hex.pm"
  @default_cdn "https://s3.amazonaws.com/s3.hex.pm"
  @default_home "~/.hex"

  @logged_keys ["http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY"]

  def start do
    unless Application.get_env(:hex, :started) do
      start_api()
      start_mix()
    else
      Application.put_env(:hex, :started, true)
    end
  end

  defp start_api do
    :ssl.start()
    :inets.start()
    :inets.start(:httpc, profile: :hex)

    config = Hex.Config.read
    if home = System.get_env("HEX_HOME"), do: home(home)
    config(config, ["HEX_API"], :api_url, &url/1)
    config(config, ["HEX_CDN"], :cdn_url, &cdn/1)
    config(config, ["http_proxy", "HTTP_PROXY"], :http_proxy, &proxy/1)
    config(config, ["https_proxy", "HTTPS_PROXY"], :https_proxy, &proxy/1)

    http_opts()

    Hex.Parallel.start_link(:hex_fetcher, max_parallel: 8)
  end

  defp start_mix do
    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)
  end

  def url do
    Application.get_env(:hex, :url) || @default_url
  end

  def url(url) do
    url = String.rstrip(url, ?/)
    Application.put_env(:hex, :url, url)
  end

  def cdn do
    Application.get_env(:hex, :cdn) || @default_cdn
  end

  def cdn(cdn) do
    cdn = String.rstrip(cdn, ?/)
    Application.put_env(:hex, :cdn, cdn)
  end

  def home do
    Application.get_env(:hex, :home) || Path.expand(@default_home)
  end

  def home(home) do
    home = Path.expand(home)
    Application.put_env(:hex, :home, home)
  end

  def version,        do: unquote(Mix.Project.config[:version])
  def elixir_version, do: unquote(System.version)

  defp config(config, envs, config_key, fun) do
    result = envs 
             |> Enum.map(& exists(:env, &1) )
             |> Enum.find(& not is_nil &1)
    result = result || exists(:config, config_key)

    if result do
      {key, value} = result

      log_value(key, value)
      fun.(value)
    end
  end


  defp exists(:env, key) do
    if value = System.get_env(key) do
      {key, value}
    else
      nil
    end
  end

  defp exists(:config, key) do
    config = Hex.Config.read

    if value = Keyword.get(config, key) do
      {Atom.to_string(key), value}
    else
      nil
    end
  end


  defp log_value(key, value) do
    if Enum.member?(@logged_keys, key) do
      Mix.shell.info("Using #{key} = #{value}")
    end
  end

  defp proxy(proxy) do
    uri = URI.parse(proxy)

    if uri.host && uri.port do
      host = String.to_char_list(uri.host)
      :httpc.set_options([{proxy_scheme(uri.scheme), {{host, uri.port}, []}}], :hex)
    end
  end

  defp proxy_scheme(scheme) do
    case scheme do
      "http" -> :proxy
      "https" -> :https_proxy
    end
  end

  defp http_opts do
    opts = [
      max_sessions: 4,
      max_keep_alive_length: 4,
      keep_alive_timeout: 120_000,
      max_pipeline_length: 4,
      pipeline_timeout: 60_000,
    ]

    :httpc.set_options(opts, :hex)
  end
end
