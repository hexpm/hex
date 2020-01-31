defmodule Hex.API.Release do
  @moduledoc false

  alias Hex.API

  def get(repo, name, version, auth \\ []) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}"
    API.request(:get, repo, path, auth)
  end

  def publish(repo, tar, auth, progress \\ fn _ -> nil end, replace \\ false)

  def publish(repo, tar, auth, progress, replace?) do
    path = "publish?replace=#{replace?}"
    opts = [progress: progress] ++ auth
    API.tar_post_request(repo, path, tar, opts)
  end

  def delete(repo, name, version, auth) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}"
    API.request(:delete, repo, path, auth)
  end

  def retire(repo, name, version, body, auth) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/retire"
    API.erlang_post_request(repo, path, body, auth)
  end

  def unretire(repo, name, version, auth) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/retire"
    API.request(:delete, repo, path, auth)
  end
end
