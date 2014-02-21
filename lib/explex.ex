defmodule Explex do
  def start_api do
    :inets.start()
    if url = System.get_env("EXPLEX_URL") do
      url(url)
    end
  end

  def start_mix do
    Explex.Registry.start()
    Mix.SCM.append(Explex.SCM)
    Mix.RemoteConverger.register(Explex.RemoteConverger)
  end

  def stop do
    Explex.Registry.stop
  end

  def url do
    case :application.get_env(:explex, :url) do
      { :ok, url } -> url
      :undefined   -> "http://localhost:4000" # "http://explex.org"
    end
  end

  def url(url) do
    url = String.rstrip(url, ?/)
    :application.set_env(:explex, :url, url)
  end
end
