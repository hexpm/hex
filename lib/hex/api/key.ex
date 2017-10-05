defmodule Hex.API.Key do
  alias Hex.API

  def new(name, permissions \\ nil, auth) do
    permissions = permissions || [%{"domain" => "api"}]
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
