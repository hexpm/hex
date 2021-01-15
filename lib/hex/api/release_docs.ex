defmodule Hex.API.ReleaseDocs do
  @moduledoc false

  alias Hex.API

  def get(repo, name, version) do
    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs"
    API.request(:get, repo, path)
  end

  def publish(repo, name, version, tar, auth, progress \\ fn _ -> nil end) do
    Hex.API.check_write_api()

    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs"
    opts = [progress: progress] ++ auth
    API.tar_post_request(repo, path, tar, opts)
  end

  def delete(repo, name, version, auth) do
    Hex.API.check_write_api()

    path = "packages/#{URI.encode(name)}/releases/#{URI.encode(version)}/docs"
    API.request(:delete, repo, path, auth)
  end
end
