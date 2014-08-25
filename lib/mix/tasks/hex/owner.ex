defmodule Mix.Tasks.Hex.Owner do
  use Mix.Task
  alias Mix.Tasks.Hex.Util

  @shortdoc "Hex package ownership tasks"

  @moduledoc """
  Add, remove or list package owners.

  A package owner have full permissions to the package. They can publish and
  revert releases and even remove other package owners.

  ### Add owner

  Add an owner to package by specifying the package name and email of the new
  owner.

  `mix hex.owner add PACKAGE EMAIL`

  ### Remove owner

  Remove an owner to package by specifying the package name and email of the new
  owner.

  `mix hex.owner remove PACKAGE EMAIL`

  ### List owners

  List all owners of given package.

  `mix hex.owner list PACKAGE`
  """

  def run(args) do
    Hex.Util.ensure_registry(fetch: false)
    Hex.start_api

    auth = Util.auth_info()

    case args do
      ["add", package, owner] ->
        add_owner(package, owner, auth)
      ["remove", package, owner] ->
        remove_owner(package, owner, auth)
      ["list", package] ->
        list_owners(package, auth)
      _ ->
        Mix.raise "Invalid arguments, expected one of:\nmix hex.owner add PACKAGE EMAIL\n" <>
                  "mix hex.owner remove PACKAGE EMAIL\nmix hex.owner list PACKAGE"
    end
  end

  defp add_owner(package, owner, opts) do
    Mix.shell.info("Adding owner #{owner} to #{package}")
    case Hex.API.Package.Owner.add(package, owner, opts) do
      {204, _body} ->
        :ok
      {code, body} ->
        Mix.shell.error("Adding owner failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp remove_owner(package, owner, opts) do
    Mix.shell.info("Removing owner #{owner} from #{package}")
    case Hex.API.Package.Owner.delete(package, owner, opts) do
      {204, _body} ->
        :ok
      {code, body} ->
        Mix.shell.error("Removing owner failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end

  defp list_owners(package, opts) do
    case Hex.API.Package.Owner.get(package, opts) do
      {200, body} ->
        Enum.each(body, &Mix.shell.info(&1["email"]))
      {code, body} ->
        Mix.shell.error("Package owner fetching failed (#{code})")
        Hex.Util.print_error_result(code, body)
    end
  end
end
