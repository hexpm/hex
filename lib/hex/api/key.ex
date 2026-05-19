defmodule Hex.API.Key do
  @moduledoc false

  alias Hex.API.Client

  def new(name, permissions, auth) do
    Mix.Tasks.Hex.with_otp_retry(auth, fn auth_with_otp ->
      config = Client.config(auth_with_otp)

      # Convert permissions to binary map format expected by hex_core
      permissions =
        Enum.map(permissions, fn perm ->
          Map.new(perm, fn {k, v} -> {to_string(k), to_string(v)} end)
        end)

      :mix_hex_api_key.add(config, to_string(name), permissions)
    end)
  end

  def get(auth) do
    config = Client.config(auth)
    :mix_hex_api_key.list(config)
  end

  def delete(name, auth) do
    Mix.Tasks.Hex.with_otp_retry(auth, fn auth_with_otp ->
      config = Client.config(auth_with_otp)
      :mix_hex_api_key.delete(config, to_string(name))
    end)
  end

  def delete_all(auth) do
    Mix.Tasks.Hex.with_otp_retry(auth, fn auth_with_otp ->
      config = Client.config(auth_with_otp)
      :mix_hex_api_key.delete_all(config)
    end)
  end

  defmodule Organization do
    @moduledoc false

    alias Hex.API.Client

    def new(organization, name, permissions, auth) do
      Mix.Tasks.Hex.with_otp_retry(auth, fn auth_with_otp ->
        config =
          Client.config(Keyword.put(auth_with_otp, :api_organization, to_string(organization)))

        # Convert permissions to binary map format expected by hex_core
        permissions =
          Enum.map(permissions, fn perm ->
            Map.new(perm, fn {k, v} -> {to_string(k), to_string(v)} end)
          end)

        :mix_hex_api_key.add(config, to_string(name), permissions)
      end)
    end

    def get(organization, auth) do
      config = Client.config(Keyword.put(auth, :api_organization, to_string(organization)))
      :mix_hex_api_key.list(config)
    end

    def delete(organization, name, auth) do
      Mix.Tasks.Hex.with_otp_retry(auth, fn auth_with_otp ->
        config =
          Client.config(Keyword.put(auth_with_otp, :api_organization, to_string(organization)))

        :mix_hex_api_key.delete(config, to_string(name))
      end)
    end

    def delete_all(organization, auth) do
      Mix.Tasks.Hex.with_otp_retry(auth, fn auth_with_otp ->
        config =
          Client.config(Keyword.put(auth_with_otp, :api_organization, to_string(organization)))

        :mix_hex_api_key.delete_all(config)
      end)
    end
  end
end
