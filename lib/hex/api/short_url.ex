defmodule Hex.API.ShortURL do
  @moduledoc false

  alias Hex.API.Client

  def create(url) do
    config = Client.config()

    case :mix_hex_api_short_url.create(config, to_string(url)) do
      {:ok, {201, _headers, %{"url" => short_url}}} ->
        {:ok, short_url}
      _error ->
        :error
    end
  end
end
