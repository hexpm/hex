defmodule Hex.API.Release do
  @moduledoc false

  alias Hex.API.Client

  def get(repo, name, version, auth \\ []) do
    config = Client.build_config(repo, auth)

    Hex.Auth.with_api(
      :read,
      config,
      &:mix_hex_api_release.get(&1, to_string(name), to_string(version)),
      auth_inline: false,
      optional: true
    )
  end

  def publish(repo, tar, auth \\ [], progress \\ fn _ -> nil end, replace \\ false)

  def publish(repo, tar, auth, progress, replace?) do
    config = Client.build_config(repo, auth)
    # Pass progress callback through adapter config
    adapter_config = %{progress_callback: progress}

    Hex.Auth.with_api(:write, config, fn config ->
      config = Map.put(config, :http_adapter, {Hex.HTTP, adapter_config})

      :mix_hex_api_release.publish(config, tar, replace: replace?)
    end)
  end

  def delete(repo, name, version, auth \\ []) do
    config = Client.build_config(repo, auth)

    Hex.Auth.with_api(
      :write,
      config,
      &:mix_hex_api_release.delete(&1, to_string(name), to_string(version))
    )
  end

  def retire(repo, name, version, body, auth \\ []) do
    config = Client.build_config(repo, auth)

    # Convert body to binary map for hex_core
    params = Map.new(body, fn {k, v} -> {to_string(k), to_string(v)} end)

    Hex.Auth.with_api(
      :write,
      config,
      &:mix_hex_api_release.retire(&1, to_string(name), to_string(version), params)
    )
  end

  def unretire(repo, name, version, auth \\ []) do
    config = Client.build_config(repo, auth)

    Hex.Auth.with_api(
      :write,
      config,
      &:mix_hex_api_release.unretire(&1, to_string(name), to_string(version))
    )
  end
end
