defmodule Hex.API.Key do
  alias Hex.API

  def new(repo, name, permissions \\ nil, auth) do
    permissions = permissions || [%{"domain" => "api"}]
    API.erlang_post_request(repo, "keys", %{name: name, permissions: permissions}, auth)
  end

  def get(repo, auth) do
    API.request(:get, repo, "keys", auth)
  end

  def delete(repo, name, auth) do
    API.request(:delete, repo, "keys/#{name}", auth)
  end

  def delete_all(repo, auth) do
    API.request(:delete, repo, "keys", auth)
  end
end
