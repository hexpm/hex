defmodule Hex.API.Package do
  @moduledoc false

  alias Hex.API.Client

  def get(repo, name, auth \\ []) when name != "" do
    config = Client.build_config(repo, auth)

    Hex.Auth.with_api(
      :read,
      config,
      &:mix_hex_api_package.get(&1, to_string(name)),
      auth_inline: false,
      optional: true
    )
  end

  def search(repo, search, auth \\ []) do
    config = Client.build_config(repo, auth)
    search_params = [{:sort, "downloads"}]

    Hex.Auth.with_api(
      :read,
      config,
      &:mix_hex_api_package.search(&1, to_string(search), search_params),
      auth_inline: false,
      optional: true
    )
  end

  defmodule Owner do
    @moduledoc false

    alias Hex.API.Client

    def add(repo, package, owner, level, transfer, auth \\ []) when package != "" do
      config =
        Client.build_config(repo, auth)

      Hex.Auth.with_api(
        :write,
        config,
        &:mix_hex_api_package_owner.add(
          &1,
          to_string(package),
          to_string(owner),
          to_string(level),
          transfer
        )
      )
    end

    def delete(repo, package, owner, auth \\ []) when package != "" do
      config = Client.build_config(repo, auth)

      Hex.Auth.with_api(
        :write,
        config,
        &:mix_hex_api_package_owner.delete(
          &1,
          to_string(package),
          to_string(owner)
        )
      )
    end

    def get(repo, package, auth \\ []) when package != "" do
      config = Client.build_config(repo, auth)

      Hex.Auth.with_api(
        :read,
        config,
        &:mix_hex_api_package_owner.list(&1, to_string(package)),
        auth_inline: false,
        optional: true
      )
    end
  end
end
