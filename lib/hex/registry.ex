defmodule Hex.Registry do
  @moduledoc false

  @type repo :: String.t()
  @type package :: String.t()
  @type version :: String.t()
  @type requirement :: String.t()
  @type app :: String.t()
  @type optional :: boolean

  @callback prefetch([{repo, package}]) :: :ok
  @callback versions(repo, package) :: [version] | nil
  @callback deps(repo, package, version) :: [{repo, package, app, requirement, optional}] | nil
end
