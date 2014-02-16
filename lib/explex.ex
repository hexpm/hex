defmodule Explex do
  def start() do
    :inets.start()

    if url = System.get_env("EXPLEX_URL") do
      url(url)
    end

    Explex.Registry.start()
    Mix.SCM.append(Explex.SCM)
    Mix.RemoteConverger.register(Explex.RemoteConverger)
  end

  def stop do
    Explex.Registry.stop
    :inets.stop()
  end

  def url do
    case :application.get_env(:explex, :url) do
      { :ok, url } -> url
      :undefined   -> "http://explex.org"
    end
  end

  def url(url) do
    url = String.rstrip(url, ?/)
    :application.set_env(:explex, :url, url)
  end
end
