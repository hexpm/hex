defmodule Hex.API.Release do
  alias Hex.API

  def get(name, version) do
    url = API.api_url("packages/#{name}/releases/#{version}")
    API.request(:get, url, [])
  end

  def new(name, tar, auth, progress \\ fn _ -> end) do
    url = API.api_url("packages/#{name}/releases")
    API.request_tar(:post, url, API.auth(auth), tar, progress)
  end

  def delete(name, version, auth) do
    url = API.api_url("packages/#{name}/releases/#{version}")
    API.request(:delete, url, API.auth(auth))
  end

  def get_docs(name, version) do
    url = API.api_url("packages/#{name}/releases/#{version}/docs")
    API.request(:get, url, [])
  end

  def new_docs(name, version, tar, auth, progress \\ fn _ -> end) do
    url = API.api_url("packages/#{name}/releases/#{version}/docs")
    API.request_tar(:post, url, API.auth(auth), tar, progress)
  end

  def delete_docs(name, version, auth) do
    url = API.api_url("packages/#{name}/releases/#{version}/docs")
    API.request(:delete, url, API.auth(auth))
  end
end
