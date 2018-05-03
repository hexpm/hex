defmodule Hex.API.Key do
  alias Hex.API

  def new(name, permissions, auth) do
    API.erlang_post_request(nil, "keys", %{name: name, permissions: permissions}, auth)
  end

  def get(auth) do
    API.request(:get, nil, "keys", auth)
  end

  def delete(name, auth) do
    API.request(:delete, nil, "keys/#{URI.encode(name)}", auth)
  end

  def delete_all(auth) do
    API.request(:delete, nil, "keys", auth)
  end
end
