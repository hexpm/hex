defmodule Hex.API.ReleaseDocs do
  alias Hex.API

  def get(name, version) do
    API.request(:get, "packages/#{name}/releases/#{version}/docs")
  end

  def new(name, version, tar, auth, progress \\ fn _ -> nil end) do
    API.tar_post_request("packages/#{name}/releases/#{version}/docs", tar, [progress: progress] ++ auth)
  end

  def delete(name, version, auth) do
    API.request(:delete, "packages/#{name}/releases/#{version}/docs", auth)
  end
end
