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

  ## Command line options

    * `--organization ORGANIZATION` - The organization the package belongs to
  """

  @switches [organization: :string]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    organization = opts[:organization]

    case args do
      ["add", package, owner] ->
        add_owner(organization, package, owner)
      ["remove", package, owner] ->
        remove_owner(organization, package, owner)
      ["list", package] ->
        list_owners(organization, package)
      ["packages"] ->
        list_owned_packages()
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

  defp add_owner(organization, package, owner) do
    auth = Mix.Tasks.Hex.auth_info()
    Hex.Shell.info "Adding owner #{owner} to #{package}"

    case Hex.API.Package.Owner.add(organization, package, owner, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok
      other ->
        Hex.Shell.error "Adding owner failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp remove_owner(organization, package, owner) do
    auth = Mix.Tasks.Hex.auth_info()
    Hex.Shell.info "Removing owner #{owner} from #{package}"

    case Hex.API.Package.Owner.delete(organization, package, owner, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok
      other ->
        Hex.Shell.error "Removing owner failed"
        Hex.Utils.print_error_result(other)
    end
  end

  defp list_owners(organization, package) do
    auth = Mix.Tasks.Hex.auth_info()

    case Hex.API.Package.Owner.get(organization, package, auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        Enum.each(body, &Hex.Shell.info(&1["email"]))
      other ->
        Hex.Shell.error "Package owner fetching failed"
        Hex.Utils.print_error_result(other)
    end
  end

  def list_owned_packages() do
    auth = Mix.Tasks.Hex.auth_info()

    case Hex.API.User.me(auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        Enum.each(body["owned_packages"], fn {name, _url} ->
          Hex.Shell.info("#{name} - #{url(name)}")
        end)
      other ->
        Hex.Shell.error("Listing owned packages failed")
        Hex.Utils.print_error_result(other)
    end
  end

  # TODO: Use html_url
  defp url(name) do
    "https://hex.pm/packages/#{name}"
  end
end
