defmodule Hex.API.Release do
  @moduledoc false

  alias Hex.API.Client

  def get(repo, name, version, auth \\ []) do
    config = build_config(repo, auth)

    :mix_hex_api_release.get(config, to_string(name), to_string(version))
  end

  def publish(repo, tar, auth, progress \\ fn _ -> nil end, replace \\ false)

  def publish(repo, tar, auth, progress, replace?) do
    config = build_config(repo, auth)

    # Pass progress callback through adapter config
    adapter_config = %{progress_callback: progress}
    config = Map.put(config, :http_adapter, {Hex.HTTP, adapter_config})

    params = [{:replace, replace?}]
    :mix_hex_api_release.publish(config, tar, params)
  end

  def delete(repo, name, version, auth) do
    config = build_config(repo, auth)

    :mix_hex_api_release.delete(config, to_string(name), to_string(version))
  end

  def retire(repo, name, version, body, auth) do
    config = build_config(repo, auth)
    # Convert body to binary map for hex_core
    params = Map.new(body, fn {k, v} -> {to_string(k), to_string(v)} end)

    :mix_hex_api_release.retire(config, to_string(name), to_string(version), params)
  end

  def unretire(repo, name, version, auth) do
    config = build_config(repo, auth)

    :mix_hex_api_release.unretire(config, to_string(name), to_string(version))
  end

  defp build_config(repo, auth) do
    opts = if repo, do: [repository: to_string(repo)], else: []
    opts = if auth, do: Keyword.merge(opts, auth), else: opts
    Client.config(opts)
  end
end
