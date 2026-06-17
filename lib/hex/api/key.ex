defmodule Hex.API.Key do
  @moduledoc false

  alias Hex.API.Client

  def new(name, permissions, auth \\ []) do
    config = Client.config(auth)

    # Convert permissions to binary map format expected by hex_core
    permissions =
      Enum.map(permissions, fn perm ->
        Map.new(perm, fn {k, v} -> {to_string(k), to_string(v)} end)
      end)

    Hex.Auth.with_api(:write, config, &:mix_hex_api_key.add(&1, to_string(name), permissions))
  end

  def get(auth \\ []) do
    config = Client.config(auth)

    Hex.Auth.with_api(:read, config, &:mix_hex_api_key.list(&1))
  end

  def delete(name, auth \\ []) do
    config = Client.config(auth)

    Hex.Auth.with_api(:write, config, &:mix_hex_api_key.delete(&1, to_string(name)))
  end

  def delete_all(auth \\ []) do
    config = Client.config(auth)

    Hex.Auth.with_api(:write, config, &:mix_hex_api_key.delete_all(&1))
  end

  defmodule Organization do
    @moduledoc false

    alias Hex.API.Client

    def new(organization, name, permissions, auth \\ []) do
      config =
        Client.config(Keyword.put(auth, :api_organization, to_string(organization)))

      # Convert permissions to binary map format expected by hex_core
      permissions =
        Enum.map(permissions, fn perm ->
          Map.new(perm, fn {k, v} -> {to_string(k), to_string(v)} end)
        end)

      Hex.Auth.with_api(:write, config, &:mix_hex_api_key.add(&1, to_string(name), permissions))
    end

    def get(organization, auth \\ []) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))
      Hex.Auth.with_api(:read, config, &:mix_hex_api_key.list(&1))
    end

    def delete(organization, name, auth \\ []) do
      config =
        Client.config(Keyword.put(auth, :api_organization, to_string(organization)))

      Hex.Auth.with_api(:write, config, &:mix_hex_api_key.delete(&1, to_string(name)))
    end

    def delete_all(organization, auth \\ []) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))

      Hex.Auth.with_api(:write, config, &:mix_hex_api_key.delete_all(&1))
    end
  end
end
