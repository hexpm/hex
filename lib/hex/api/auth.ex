defmodule Hex.API.Auth do
  @moduledoc false

  alias Hex.API.Client

  def get(domain, resource, auth) do
    config = Client.config(auth)

    params = %{
      domain: to_string(domain),
      resource: to_string(resource)
    }

    :mix_hex_api_auth.test(config, params)
  end
end
