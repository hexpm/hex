defmodule Hex.API.ReleaseDocs do
  @moduledoc false

  alias Hex.API.Client

  def get(repo, name, version) do
    config = build_config(repo, [])
    path = :mix_hex_api.build_repository_path(config, ["packages", to_string(name), "releases", to_string(version), "docs"])

    :mix_hex_api.get(config, path)
  end

  def publish(repo, name, version, tar, auth, _progress \\ fn _ -> nil end) do
    config = build_config(repo, auth)
    path = :mix_hex_api.build_repository_path(config, ["packages", to_string(name), "releases", to_string(version), "docs"])

    body = {"application/octet-stream", tar}

    # TODO: Progress callback needs to be handled differently
    :mix_hex_api.post(config, path, body)
  end

  def delete(repo, name, version, auth) do
    config = build_config(repo, auth)
    path = :mix_hex_api.build_repository_path(config, ["packages", to_string(name), "releases", to_string(version), "docs"])

    :mix_hex_api.delete(config, path)
  end

  defp build_config(repo, auth) do
    opts = if repo, do: [repository: to_string(repo)], else: []
    opts = if auth, do: Keyword.merge(opts, auth), else: opts
    Client.config(opts)
  end

end
