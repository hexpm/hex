defmodule Hex.API.Key do
  @moduledoc false

  alias Hex.API

  def new(name, permissions, auth) do
    Hex.API.check_write_api()
    API.erlang_post_request(nil, "keys", %{name: name, permissions: permissions}, auth)
  end

  def get(auth) do
    API.request(:get, nil, "keys", auth)
  end

  def delete(name, auth) do
    Hex.API.check_write_api()
    API.request(:delete, nil, "keys/#{URI.encode(name)}", auth)
  end

  def delete_all(auth) do
    Hex.API.check_write_api()
    API.request(:delete, nil, "keys", auth)
  end

  defmodule Organization do
    @moduledoc false

    def new(organization, name, permissions, auth) do
      Hex.API.check_write_api()

      API.erlang_post_request(
        nil,
        "orgs/#{organization}/keys",
        %{name: name, permissions: permissions},
        auth
      )
    end

    def get(organization, auth) do
      API.request(:get, nil, "orgs/#{organization}/keys", auth)
    end

    def delete(organization, name, auth) do
      Hex.API.check_write_api()

      API.request(:delete, nil, "orgs/#{organization}/keys/#{URI.encode(name)}", auth)
    end

    def delete_all(organization, auth) do
      Hex.API.check_write_api()

      API.request(:delete, nil, "orgs/#{organization}/keys", auth)
    end
  end
end
