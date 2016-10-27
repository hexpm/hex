defmodule Hex.API.User do
  alias Hex.API

  def get(username) do
    API.request(:get, API.api_url("users/#{username}"), [])
  end

  def test(username, auth) do
    API.request(:get, API.api_url("users/#{username}/test"), API.auth(auth))
  end

  def new(username, email, password) do
    API.request(:post, API.api_url("users"), [],
            %{username: username, email: email, password: password})
  end

  def password_reset(name) do
    API.request(:post, API.api_url("/users/#{name}/reset"), [], %{})
  end
end
