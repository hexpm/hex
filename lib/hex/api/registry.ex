defmodule Hex.API.Registry do
  alias Hex.API

  def get(opts \\ []) do
    headers =
      if etag = opts[:etag] do
        %{'if-none-match' => etag}
      end

    API.request(:get, API.cdn_url("registry.ets.gz"), headers || [])
  end
end
