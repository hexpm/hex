defmodule Hex do
  def start do
    start_api()
    start_mix()
  end

  def start_api do
    :ssl.start()
    :inets.start()
    if url = System.get_env("HEX_URL"), do: url(url)
  end

  def start_mix do
    Hex.Registry.start()
    Mix.SCM.append(Hex.SCM)
    Mix.RemoteConverger.register(Hex.RemoteConverger)
  end

  def stop do
    Hex.Registry.stop
  end

  def url do
    case :application.get_env(:hex, :url) do
      { :ok, url } -> url
      :undefined   -> "https://hex.pm"
    end
  end

  def url(url) do
    url = String.rstrip(url, ?/)
    :application.set_env(:hex, :url, url)
  end

  version = Mix.project[:version]
  { :ok, Version.Schema[pre: pre] } = Version.parse(version)
  channel = Enum.join(pre, ".")

  def version, do: unquote(version)
  def channel, do: unquote(channel)
end
