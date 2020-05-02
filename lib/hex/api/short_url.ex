defmodule Hex.API.ShortURL do
  @moduledoc false

  alias Hex.API

  def create(url) do
    case API.erlang_post_request(nil, "short_url", %{url: url}) do
      {:ok, {201, %{"url" => short_url}, _resp_headers}} -> short_url
      _error -> :error
    end
  end
end
