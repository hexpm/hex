defmodule Hex do
  @default_url "https://hex.pm"
  @default_cdn "http://s3.hex.pm"


  def start do
    start_api()
    start_mix()
  end

  def start_api do
    :ssl.start()
    :inets.start()

    if url = System.get_env("HEX_URL"), do: url(url)
    if cdn = System.get_env("HEX_CDN"), do: cdn(cdn)
    if http_proxy = System.get_env("HTTP_PROXY"), do: proxy(http_proxy)
    if https_proxy = System.get_env("HTTPS_PROXY"), do: proxy(https_proxy)
  end

  def start_mix do
    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)
  end

  def url do
    case :application.get_env(:hex, :url) do
      { :ok, url } -> url
      :undefined   -> @default_url
    end
  end

  def url(url) do
    url = String.rstrip(url, ?/)
    :application.set_env(:hex, :url, url)
  end

  def cdn do
    case :application.get_env(:hex, :cdn) do
      { :ok, cdn } -> cdn
      :undefined   -> @default_cdn
    end
  end

  def cdn(cdn) do
    cdn = String.rstrip(cdn, ?/)
    :application.set_env(:hex, :cdn, cdn)
  end

  def version, do: unquote(Mix.Project.config[:version])

  defp proxy(proxy) do
    uri = URI.parse(proxy)
    :httpc.set_options [{ proxy_scheme(uri.scheme),
        { { uri.host |> String.to_char_list, uri.port }, [] } }]
  end

  defp proxy_scheme(scheme) do
    case scheme do
      "http" -> :proxy
      "https" -> :https_proxy
    end
  end
end
