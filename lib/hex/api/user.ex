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

  # NOTE: Only used for testing
  def new(username, email, password) do
    config = Client.config()

    :mix_hex_api_user.create(
      config,
      to_string(username),
      to_string(password),
      to_string(email)
    )
  end
end
