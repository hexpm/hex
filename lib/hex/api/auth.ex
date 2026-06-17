defmodule Hex.API.Auth do
  @moduledoc false

  alias Hex.API.Client

  def get(domain, resource, auth \\ []) do
    config = Client.config(auth)

    params = %{
      domain: to_string(domain),
      resource: to_string(resource)
    }

    Hex.Auth.with_api(:read, config, &:mix_hex_api_auth.test(&1, params),
      auth_inline: false,
      optional: true
    )
  end
end
