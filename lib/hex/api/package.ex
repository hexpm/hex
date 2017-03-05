defmodule Hex.API.Package do
  alias Hex.API

  def get(name) do
    API.request(:get, "packages/#{name}")
  end

  def search(search) do
    API.request(:get, "packages?search=#{search}&sort=downloads")
  end

  defmodule Owner do
    def add(package, owner, auth) do
      owner = URI.encode_www_form(owner)
      API.erlang_put_request("packages/#{package}/owners/#{owner}", %{}, auth)
    end

    def delete(package, owner, auth) do
      owner = URI.encode_www_form(owner)
      API.request(:delete, "packages/#{package}/owners/#{owner}", auth)
    end

    def get(package, auth) do
      API.request(:get, "packages/#{package}/owners", auth)
    end
  end
end
