defmodule Hex.API.User do
  @moduledoc false

  alias Hex.API

  def me(auth) do
    API.request(:get, nil, "users/me", auth)
  end

  def get(username) do
    API.request(:get, nil, "users/#{URI.encode(username)}")
  end

  def new(username, email, password) do
    Hex.API.check_write_api()

    API.erlang_post_request(nil, "users", %{username: username, email: email, password: password})
  end

  def password_reset(name) do
    Hex.API.check_write_api()

    API.erlang_post_request(nil, "users/#{URI.encode(name)}/reset", %{})
  end
end
