defmodule Hex.API.Registry do
  alias Hex.API

  def get(opts \\ []) do
    if etag = opts[:etag] do
      headers = %{'if-none-match' => etag}
    end
    API.request(:get, API.cdn_url("registry.ets.gz"), headers || [])
  end
end
