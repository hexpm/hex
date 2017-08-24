defmodule Hex.API.Release do
  alias Hex.API

  def get(repo, name, version) do
    API.request(:get, repo, "packages/#{name}/releases/#{version}")
  end

  def new(repo, name, tar, auth, progress \\ fn _ -> nil end) do
    API.tar_post_request(repo, "packages/#{name}/releases", tar, [progress: progress] ++ auth)
  end

  def delete(repo, name, version, auth) do
    API.request(:delete, repo, "packages/#{name}/releases/#{version}", auth)
  end

  def retire(repo, name, version, body, auth) do
    API.erlang_post_request(repo, "packages/#{name}/releases/#{version}/retire", body, auth)
  end

  def unretire(repo, name, version, auth) do
    API.request(:delete, repo, "packages/#{name}/releases/#{version}/retire", auth)
  end
end
