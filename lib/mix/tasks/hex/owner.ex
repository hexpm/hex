defmodule Mix.Tasks.Hex.Owner do
  use Mix.Task
  alias Mix.Hex.Utils

  @shortdoc "Manages Hex package ownership"

  @moduledoc """
  Adds, removes or lists package owners.

  Package owners have full permissions to the package. They can publish and
  revert releases and even remove other package owners.

  ### Add owner

  Adds an owner to package by specifying the package name and email of the new
  owner.

      mix hex.owner add PACKAGE EMAIL

  ### Remove owner

  Removes an owner to package by specifying the package name and email of the new
  owner.

      mix hex.owner remove PACKAGE EMAIL

  ### List owners

  Lists all owners of given package.

      mix hex.owner list PACKAGE

  ### List owned packages

  Lists all packages owned by the current user.

      mix hex.owner packages
  """

  def run(args) do
    Hex.start
    config = Hex.Config.read
    auth = Utils.auth_info(config)

    case args do
      ["add", package, owner] ->
        add_owner(package, owner, auth)
      ["remove", package, owner] ->
        remove_owner(package, owner, auth)
      ["list", package] ->
        list_owners(package, auth)
      ["packages"] ->
        list_owned_packages(config, auth)
      _ ->
        Mix.raise "Invalid arguments, expected one of:\nmix hex.owner add PACKAGE EMAIL\n" <>
                  "mix hex.owner remove PACKAGE EMAIL\nmix hex.owner list PACKAGE\n" <>
                  "mix hex.owner packages"
    end
  end

  defp add_owner(package, owner, opts) do
    Hex.Shell.info "Adding owner #{owner} to #{package}"
    case Hex.API.Package.Owner.add(package, owner, opts) do
      {code, _body, _headers} when code in 200..299 ->
        :ok
      {code, body, _headers} ->
        Hex.Shell.error "Adding owner failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp remove_owner(package, owner, opts) do
    Hex.Shell.info "Removing owner #{owner} from #{package}"
    case Hex.API.Package.Owner.delete(package, owner, opts) do
      {code, _body, _headers} when code in 200..299 ->
        :ok
      {code, body, _headers} ->
        Hex.Shell.error "Removing owner failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  defp list_owners(package, opts) do
    case Hex.API.Package.Owner.get(package, opts) do
      {code, body, _headers} when code in 200..299 ->
        Enum.each(body, &Hex.Shell.info(&1["email"]))
      {code, body, _headers} ->
        Hex.Shell.error "Package owner fetching failed"
        Hex.Utils.print_error_result(code, body)
    end
  end

  def list_owned_packages(config, auth) do
    {:ok, username} = Keyword.fetch(config, :username)
    case Hex.API.User.get(username, auth) do
      {code, body, _headers} when code in 200..299 ->
        Enum.each(body["owned_packages"], fn({name, url}) ->
          Hex.Shell.info("#{name} - #{url}")
        end)
      {code, body, _headers} ->
        Hex.Shell.error("Listing owned packages failed")
        Hex.Utils.print_error_result(code, body)
    end
  end
end
