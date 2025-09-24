defmodule Hex.API.Package do
  @moduledoc false

  alias Hex.API.Client

  def get(repo, name, auth \\ []) when name != "" do
    config = Client.build_config(repo, auth)
    :mix_hex_api_package.get(config, to_string(name))
  end

  def search(repo, search, auth \\ []) do
    config = Client.build_config(repo, auth)
    search_params = [{:sort, "downloads"}]

    :mix_hex_api_package.search(config, to_string(search), search_params)
  end

  defmodule Owner do
    @moduledoc false

    alias Hex.API.Client

    def add(repo, package, owner, level, transfer, auth) when package != "" do
      config = Client.build_config(repo, auth)

      :mix_hex_api_package_owner.add(
        config,
        to_string(package),
        to_string(owner),
        to_string(level),
        transfer
      )
    end

    def delete(repo, package, owner, auth) when package != "" do
      config = Client.build_config(repo, auth)

      :mix_hex_api_package_owner.delete(
        config,
        to_string(package),
        to_string(owner)
      )
    end

    def get(repo, package, auth) when package != "" do
      config = Client.build_config(repo, auth)
      :mix_hex_api_package_owner.list(config, to_string(package))
    end
  end
end
