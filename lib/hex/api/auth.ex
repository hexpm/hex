defmodule Hex.API.Auth do
  @moduledoc false

  alias Hex.API

  def get(domain, resource, auth) do
    API.request(:get, nil, "auth?domain=#{domain}&resource=#{resource}", auth)
  end
end
