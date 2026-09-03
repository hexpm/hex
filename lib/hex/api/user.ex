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
end
