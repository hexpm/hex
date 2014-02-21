defmodule Hex do
  def start_api do
    :inets.start()
    if url = System.get_env("HEX_URL") do
      url(url)
    end
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
      :undefined   -> "http://localhost:4000" # "http://hex.org"
    end
  end

  def url(url) do
    url = String.rstrip(url, ?/)
    :application.set_env(:hex, :url, url)
  end
end
