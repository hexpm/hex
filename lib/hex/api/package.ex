defmodule Hex.API.Package do
  alias Hex.API

  def get(repo, name) do
    API.request(:get, repo, "packages/#{URI.encode(name)}")
  end

  def search(repo, search) do
    API.request(:get, repo, "packages?search=#{URI.encode(search)}&sort=downloads")
  end

  defmodule Owner do
    def add(repo, package, owner, auth) do
      owner = URI.encode_www_form(owner)
      API.erlang_put_request(repo, "packages/#{URI.encode(package)}/owners/#{URI.encode(owner)}", %{}, auth)
    end

    def delete(repo, package, owner, auth) do
      owner = URI.encode_www_form(owner)
      API.request(:delete, repo, "packages/#{URI.encode(package)}/owners/#{URI.encode(owner)}", auth)
    end

    def get(repo, package, auth) do
      API.request(:get, repo, "packages/#{URI.encode(package)}/owners", auth)
    end
  end
end
