defmodule Hex.API.OIDC do
  @moduledoc false

  alias Hex.API.Client

  def audience do
    :mix_hex_api.get(Client.config(), "oidc/audience")
  end
end
