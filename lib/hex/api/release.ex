defmodule Hex.API.Release do
  alias Hex.API

  def get(name, version) do
    API.request(:get, API.api_url("packages/#{name}/releases/#{version}"), [])
  end

  def new(name, tar, auth, progress \\ fn _ -> end) do
    API.request_tar(:post, API.api_url("packages/#{name}/releases"), API.auth(auth), tar, progress)
  end

  def delete(name, version, auth) do
    API.request(:delete, API.api_url("packages/#{name}/releases/#{version}"), API.auth(auth))
  end
end
