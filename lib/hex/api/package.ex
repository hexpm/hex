defmodule Hex.API.Package do
  alias Hex.API

  def get(name) do
    API.request(:get, API.api_url("packages/#{name}"), [])
  end

  def new(name, meta, auth) do
    API.request(:put, API.api_url("packages/#{name}"), API.auth(auth), %{meta: meta})
  end
end
