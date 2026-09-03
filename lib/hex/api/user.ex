defmodule Hex.API.User do
  @moduledoc false

  alias Hex.API.Client

  def me(auth \\ []) do
    config = Client.config(auth)

    Hex.Auth.with_api(:read, config, &:mix_hex_api_user.me(&1))
  end

  def get(username, auth \\ []) do
    config = Client.config(auth)

    Hex.Auth.with_api(:read, config, &:mix_hex_api_user.get(&1, to_string(username)),
      auth_inline: false,
      optional: true
    )
  end
end
