defmodule Hex.API.Package do
  alias Hex.API

  def get(repo, name) do
    API.request(:get, repo, "packages/#{name}")
  end

  def search(repo, search) do
    API.request(:get, repo, "packages?search=#{search}&sort=downloads")
  end

  defmodule Owner do
    def add(repo, package, owner, auth) do
      owner = URI.encode_www_form(owner)
      API.erlang_put_request(repo, "packages/#{package}/owners/#{URI.encode(owner)}", %{}, auth)
    end

    def delete(repo, package, owner, auth) do
      owner = URI.encode_www_form(owner)
      API.request(:delete, repo, "packages/#{package}/owners/#{URI.encode(owner)}", auth)
    end

    def get(repo, package, auth) do
      API.request(:get, repo, "packages/#{package}/owners", auth)
    end
  end
end
