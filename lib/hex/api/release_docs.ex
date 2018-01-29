defmodule Hex.API.ReleaseDocs do
  alias Hex.API

  def get(repo, name, version) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs"
    API.request(:get, repo, path)
  end

  def new(repo, name, version, tar, auth, progress \\ fn _ -> nil end) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs"
    opts = [progress: progress] ++ auth
    API.tar_post_request(repo, path, tar, opts)
  end

  def delete(repo, name, version, auth) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs"
    API.request(:delete, repo, path, auth)
  end
end
