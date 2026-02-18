defmodule Hex.API.Search do
  @moduledoc false

  def search(query_params) do
    Hex.HTTP.request(
      :get,
      "https://search.hexdocs.pm?#{URI.encode_query(query_params)}",
      %{},
      nil
    )
  end
end
