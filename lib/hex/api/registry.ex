defmodule Hex.API.Registry do
  alias Hex.API

  def get(opts \\ []) do
    headers =
      if etag = opts[:etag] do
        %{'if-none-match' => '"' ++ etag ++ '"'}
      end

    API.request(:get, API.repo_url("registry.ets.gz"), headers || [])
  end

  def get_signature() do
    API.request(:get, API.repo_url("registry.ets.gz.signed"), [])
  end
end
