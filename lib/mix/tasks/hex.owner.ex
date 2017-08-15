defmodule Mix.Tasks.Hex.Owner do
  use Mix.Task

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
    Hex.start()

    case args do
      ["add", package, owner] ->
        auth = Mix.Tasks.Hex.auth_info("hexpm")
        add_owner(package, owner, auth)
      ["remove", package, owner] ->
        auth = Mix.Tasks.Hex.auth_info("hexpm")
        remove_owner(package, owner, auth)
      ["list", package] ->
        auth = Mix.Tasks.Hex.auth_info("hexpm")
        list_owners(package, auth)
      ["packages"] ->
        auth = Mix.Tasks.Hex.auth_info("hexpm")
        list_owned_packages(auth)
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.owner add PACKAGE EMAIL
        mix hex.owner remove PACKAGE EMAIL
        mix hex.owner list PACKAGE
        mix hex.owner packages
        """
    end
  end

  defp add_owner(package, owner, opts) do
    Hex.Shell.info "Adding owner #{owner} to #{package}"

    case Hex.API.Package.Owner.add(package, owner, opts) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok
      other ->
        Hex.Shell.error "Adding owner failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp remove_owner(package, owner, opts) do
    Hex.Shell.info "Removing owner #{owner} from #{package}"

    case Hex.API.Package.Owner.delete(package, owner, opts) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok
      other ->
        Hex.Shell.error "Removing owner failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp list_owners(package, opts) do
    case Hex.API.Package.Owner.get(package, opts) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        Enum.each(body, &Hex.Shell.info(&1["email"]))
      other ->
        Hex.Shell.error "Package owner fetching failed"
        Hex.Utils.print_error_result(other)
    end
  end

  def list_owned_packages(opts) do
    case Hex.API.User.me(opts) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        Enum.each(body["owned_packages"], fn {name, _url} ->
          Hex.Shell.info("#{name} - #{url(name)}")
        end)
      other ->
        Hex.Shell.error("Listing owned packages failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp url(name) do
    "https://hex.pm/packages/#{name}"
  end
end
