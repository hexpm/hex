defmodule Hex.API.Release do
  alias Hex.API

  def get(name, version) do
    url = API.api_url("packages/#{name}/releases/#{version}")
    API.request(:get, url, [])
  end

  def new(name, tar, auth, progress \\ fn _ -> nil end) do
    url = API.api_url("packages/#{name}/releases")
    API.request_tar(url, API.auth(auth), tar, progress)
  end

  def delete(name, version, auth) do
    url = API.api_url("packages/#{name}/releases/#{version}")
    API.request(:delete, url, API.auth(auth))
  end

  def retire(name, version, body, auth) do
    url = API.api_url("packages/#{name}/releases/#{version}/retire")
    API.request(:post, url, API.auth(auth), body)
  end

  def unretire(name, version, auth) do
    url = API.api_url("packages/#{name}/releases/#{version}/retire")
    API.request(:delete, url, API.auth(auth))
  end
end
