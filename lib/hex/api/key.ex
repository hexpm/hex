defmodule Hex.API.Key do
  alias Hex.API

  def new(name, auth) do
    API.erlang_post_request("keys", %{name: name}, auth)
  end

  def get(auth) do
    API.request(:get, "keys", auth)
  end

  def delete(name, auth) do
    API.request(:delete, "keys/#{name}", auth)
  end

  def delete_all(auth) do
    API.request(:delete, "keys", auth)
  end
end
