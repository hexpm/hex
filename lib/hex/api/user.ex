defmodule Hex.API.User do
  alias Hex.API

  def me(repo, auth) do
    API.request(:get, repo, "users/me", auth)
  end

  def get(repo, username) do
    API.request(:get, repo, "users/#{username}")
  end

  def test(repo, username, auth) do
    API.request(:get, repo, "users/#{username}/test", auth)
  end

  def new(repo, username, email, password) do
    API.erlang_post_request(repo, "users", %{username: username, email: email, password: password})
  end

  def password_reset(repo, name) do
    API.erlang_post_request(repo, "users/#{name}/reset", %{})
  end
end
