defmodule Hex.API.User do
  @moduledoc false

  alias Hex.API.Client

  def me(auth) do
    config = Client.config(auth)
    :mix_hex_api_user.me(config)
  end

  def get(username) do
    config = Client.config()
    :mix_hex_api_user.get(config, to_string(username))
  end

  def new(username, email, password) do
    config = Client.config()

    :mix_hex_api_user.create(
      config,
      to_string(username),
      to_string(password),
      to_string(email)
    )
  end

  def password_reset(name) do
    config = Client.config()
    :mix_hex_api_user.reset_password(config, to_string(name))
  end
end
