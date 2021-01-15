defmodule Hex.API.Package do
  @moduledoc false

  alias Hex.API

  def get(repo, name, auth \\ []) do
    path = "packages/#{URI.encode(name)}"
    API.request(:get, repo, path, auth)
  end

  def search(repo, search, auth \\ []) do
    path = "packages?search=#{URI.encode(search)}&sort=downloads"
    API.request(:get, repo, path, auth)
  end

  defmodule Owner do
    @moduledoc false

    def add(repo, package, owner, level, transfer, auth) do
      Hex.API.check_write_api()

      owner = URI.encode_www_form(owner)
      path = "packages/#{URI.encode(package)}/owners/#{URI.encode(owner)}"
      params = %{level: level, transfer: transfer}
      API.erlang_put_request(repo, path, params, auth)
    end

    def delete(repo, package, owner, auth) do
      Hex.API.check_write_api()

      owner = URI.encode_www_form(owner)
      path = "packages/#{URI.encode(package)}/owners/#{URI.encode(owner)}"
      API.request(:delete, repo, path, auth)
    end

    def get(repo, package, auth) do
      API.request(:get, repo, "packages/#{URI.encode(package)}/owners", auth)
    end
  end
end
