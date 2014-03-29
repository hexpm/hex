defmodule Mix.Tasks.Hex.Update do
  use Mix.Task

  @install_url "http://s3.hex.pm/installs/hex.ez"

  @shortdoc "Update hex"

  @moduledoc """
  Update the hex installation.

  `mix hex.update`

  Updating the hex installation should be done periodically. The registry status can be
  checked with `mix hex.info`. The registry contains an index of all packages,
  releases and their dependencies that is used during Hex's dependency
  resolution.
  """

  def run(_args) do
    Hex.start_api

    # TODO: Check /api/installs for url

    Mix.shell.info("Updating Hex installation...")
    Mix.Task.run "local.install", [@install_url, "--force"]
    Mix.shell.info("Updating Hex was successful!")
  end
end
