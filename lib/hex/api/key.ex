defmodule Hex.API.Key do
  alias Hex.API

  def new(name, auth) do
    API.request(:post, API.api_url("keys"), API.auth(auth), %{name: name})
  end

  def get(auth) do
    API.request(:get, API.api_url("keys"), API.auth(auth))
  end

  def delete(name, auth) do
    API.request(:delete, API.api_url("keys/#{name}"), API.auth(auth))
  end

  def delete_all(auth) do
    API.request(:delete, API.api_url("keys"), API.auth(auth))
  end
end
