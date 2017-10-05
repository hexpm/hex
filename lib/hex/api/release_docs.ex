defmodule Hex.API.ReleaseDocs do
  alias Hex.API

  def get(repo, name, version) do
    API.request(:get, repo, "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs")
  end

  def new(repo, name, version, tar, auth, progress \\ fn _ -> nil end) do
    API.tar_post_request(repo, "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs", tar, [progress: progress] ++ auth)
  end

  def delete(repo, name, version, auth) do
    API.request(:delete, repo, "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs", auth)
  end
end
