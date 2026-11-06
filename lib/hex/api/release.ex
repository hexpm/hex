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

  # Reads the status URL from a staged release response. hex_core joins
  # request paths to the configured API URL, so the status URL has to be under it.
  def get_staged(url, auth \\ []) do
    config = Client.build_config(nil, auth)
    prefix = config.api_url <> "/"

    if String.starts_with?(url, prefix) do
      path = String.replace_prefix(url, prefix, "")
      Hex.Auth.with_api(:read, config, &:mix_hex_api.get(&1, path), auth_inline: false)
    else
      {:error, {:staged_url_outside_api, url}}
    end
  end

  def publish(repo, tar, auth \\ [], progress \\ fn _ -> nil end, params \\ [])

  def publish(repo, tar, auth, progress, params) do
    config = Client.build_config(repo, auth)
    # Pass progress callback through adapter config
    adapter_config = %{progress_callback: progress}

    Hex.Auth.with_api(:write, config, fn config ->
      config = Map.put(config, :http_adapter, {Hex.HTTP, adapter_config})

      :mix_hex_api_release.publish(config, tar, params)
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
