defmodule Hex.API.Key do
  @moduledoc false

  alias Hex.API.Client

  def new(name, permissions, auth) do
    config = Client.config(auth)

    # Convert permissions to binary map format expected by hex_core
    permissions =
      Enum.map(permissions, fn perm ->
        Map.new(perm, fn {k, v} -> {to_string(k), to_string(v)} end)
      end)

    :mix_hex_api_key.add(config, to_string(name), permissions)
  end

  def get(auth) do
    config = Client.config(auth)
    :mix_hex_api_key.list(config)
  end

  def delete(name, auth) do
    config = Client.config(auth)
    :mix_hex_api_key.delete(config, to_string(name))
  end

  def delete_all(auth) do
    config = Client.config(auth)
    :mix_hex_api_key.delete_all(config)
  end

  defmodule Organization do
    @moduledoc false

    alias Hex.API.Client

    def new(organization, name, permissions, auth) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))

      # Convert permissions to binary map format expected by hex_core
      permissions =
        Enum.map(permissions, fn perm ->
          Map.new(perm, fn {k, v} -> {to_string(k), to_string(v)} end)
        end)

      :mix_hex_api_key.add(config, to_string(name), permissions)
    end

    def get(organization, auth) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))
      :mix_hex_api_key.list(config)
    end

    def delete(organization, name, auth) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))
      :mix_hex_api_key.delete(config, to_string(name))
    end

    def delete_all(organization, auth) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))
      :mix_hex_api_key.delete_all(config)
    end
  end
end
