defmodule Hex do
  @default_url "https://hex.pm"
  @default_cdn "https://s3.amazonaws.com/s3.hex.pm"
  @default_home "~/.hex"


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

    if url  = System.get_env("HEX_API"),  do: url(url)
    if cdn  = System.get_env("HEX_CDN"),  do: cdn(cdn)
    if home = System.get_env("HEX_HOME"), do: home(home)

    http_proxy  = System.get_env("HTTP_PROXY")  || System.get_env("http_proxy")
    https_proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy")
    if http_proxy,  do: proxy(http_proxy)
    if https_proxy, do: proxy(https_proxy)

    Hex.Parallel.start_link(:hex_fetcher, max_parallel: 4)
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

  defp proxy(proxy) do
    uri  = URI.parse(proxy)
    host = String.to_char_list(uri.host)
    :httpc.set_options([{proxy_scheme(uri.scheme), {{host, uri.port}, []}}], :hex)
  end

  defp proxy_scheme(scheme) do
    case scheme do
      "http" -> :proxy
      "https" -> :https_proxy
    end
  end
end
