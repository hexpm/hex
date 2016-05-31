defmodule Hex.API.ReleaseDocs do
  alias Hex.API

  def get(name, version) do
    url = API.api_url("packages/#{name}/releases/#{version}/docs")
    API.request(:get, url, [])
  end

  def new(name, version, tar, auth, progress \\ fn _ -> nil end) do
    url = API.api_url("packages/#{name}/releases/#{version}/docs")
    API.request_tar(url, API.auth(auth), tar, progress)
  end

  def delete(name, version, auth) do
    url = API.api_url("packages/#{name}/releases/#{version}/docs")
    API.request(:delete, url, API.auth(auth))
  end
end
