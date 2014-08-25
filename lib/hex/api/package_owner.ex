defmodule Hex.API.Package.Owner do
  alias Hex.API

  def add(package, owner, auth) do
    owner = URI.encode_www_form(owner)
    API.request(:put, API.api_url("packages/#{package}/owners/#{owner}"), API.auth(auth))
  end

  def delete(package, owner, auth) do
    owner = URI.encode_www_form(owner)
    API.request(:delete, API.api_url("packages/#{package}/owners/#{owner}"), API.auth(auth))
  end

  def get(package, auth) do
    API.request(:get, API.api_url("packages/#{package}/owners"), API.auth(auth))
  end
end
