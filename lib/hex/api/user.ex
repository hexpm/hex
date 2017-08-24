defmodule Hex.API.User do
  alias Hex.API

  def me(auth) do
    API.request(:get, nil, "users/me", auth)
  end

  def get(username) do
    API.request(:get, nil, "users/#{username}")
  end

  def test(username, auth) do
    API.request(:get, nil, "users/#{username}/test", auth)
  end

  def new(username, email, password) do
    API.erlang_post_request(nil, "users", %{username: username, email: email, password: password})
  end

  def password_reset(name) do
    API.erlang_post_request(nil, "users/#{name}/reset", %{})
  end
end
