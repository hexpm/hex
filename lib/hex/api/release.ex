defmodule Hex.API.Release do
  alias Hex.API

  def get(name, version) do
    API.request(:get, "packages/#{name}/releases/#{version}")
  end

  def new(name, tar, auth, progress \\ fn _ -> nil end) do
    API.tar_post_request("packages/#{name}/releases", tar, [progress: progress] ++ auth)
  end

  def delete(name, version, auth) do
    API.request(:delete, "packages/#{name}/releases/#{version}", auth)
  end

  def retire(name, version, body, auth) do
    API.erlang_post_request("packages/#{name}/releases/#{version}/retire", body, auth)
  end

  def unretire(name, version, auth) do
    API.request(:delete, "packages/#{name}/releases/#{version}/retire", auth)
  end
end
