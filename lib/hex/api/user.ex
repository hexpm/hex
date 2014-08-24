defmodule Hex.API.User do
  alias Hex.API

  def get(username) do
    API.request(:get, API.api_url("users/#{username}"), [])
  end

  def new(username, email, password) do
    API.request(:post, API.api_url("users"), [],
            %{username: username, email: email, password: password})
  end

  def update(email, password, auth) do
    body = %{}
    if email, do: body = Map.merge(body, %{email: email})
    if password, do: body = Map.merge(body, %{password: password})

    headers = Dict.merge(API.auth(auth), %{'x-http-method-override' => 'PATCH'})

    API.request(:post, API.api_url("users/#{auth[:user]}"), headers, body)
  end
end
