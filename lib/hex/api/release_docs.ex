defmodule Hex.API.ReleaseDocs do
  @moduledoc false

  alias Hex.API.Client

  def get(repo, name, version) do
    config = Client.build_config(repo, [])

    path =
      :mix_hex_api.build_repository_path(config, [
        "packages",
        to_string(name),
        "releases",
        to_string(version),
        "docs"
      ])

    :mix_hex_api.get(config, path)
  end

  def publish(repo, name, version, tar, auth, progress \\ fn _ -> nil end) do
    config = Client.build_config(repo, auth)

    # Pass progress callback through adapter config
    adapter_config = %{progress_callback: progress}
    config = Map.put(config, :http_adapter, {Hex.HTTP, adapter_config})

    path =
      :mix_hex_api.build_repository_path(config, [
        "packages",
        to_string(name),
        "releases",
        to_string(version),
        "docs"
      ])

    body = {"application/octet-stream", tar}

    :mix_hex_api.post(config, path, body)
  end

  def delete(repo, name, version, auth) do
    config = Client.build_config(repo, auth)

    path =
      :mix_hex_api.build_repository_path(config, [
        "packages",
        to_string(name),
        "releases",
        to_string(version),
        "docs"
      ])

    :mix_hex_api.delete(config, path)
  end
end
