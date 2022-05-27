defmodule Mix.Tasks.Hex.Owner do
  use Mix.Task

  @shortdoc "Manages Hex package ownership"

  @moduledoc """
  Adds, removes or lists package owners.

  Package owners have full permissions to the package. They can publish and
  revert releases and even remove other package owners.

  ## Add owner

  Adds an owner to package by specifying the package name and email or username
  of the new owner. This command also takes a `--level` option, see below for more
  details.

      $ mix hex.owner add PACKAGE EMAIL_OR_USERNAME

  ## Transfer ownership

  Like `mix hex.owner add` but also removes all existing owners of the package.
  This task is required to use when transferring ownership of the package to an
  organization.

      $ mix hex.owner transfer PACKAGE EMAIL_OR_USERNAME

  ## Remove owner

  Removes an owner to package by specifying the package name and email or username
  of the new owner.

      $ mix hex.owner remove PACKAGE EMAIL_OR_USERNAME

  ## List owners

  Lists all owners of given package.

      $ mix hex.owner list PACKAGE

  ## List owned packages

  Lists all packages owned by the current user.

      $ mix hex.owner packages

  ## Command line options

    * `--level maintainer` - Maintainer level owners have all the powers of package ownership,
      except they cannot add or remove other package owners
    * `--level full` - Over the maintainer level, full owners can also add and remove other
      package owners (default)
    * `--organization ORGANIZATION` - Set this for private packages belonging to an organization
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [organization: :string, level: :string]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, strict: @switches)
    organization = opts[:organization]
    level = opts[:level] || "full"

    case args do
      ["add", package, owner] ->
        add_owner(organization, package, owner, level)

      ["transfer", package, owner] ->
        transfer_owner(organization, package, owner)

      ["remove", package, owner] ->
        remove_owner(organization, package, owner)

      ["list", package] ->
        list_owners(organization, package)

      ["packages"] ->
        list_owned_packages()

      _ ->
        Mix.raise("""
        Invalid arguments, expected one of:

        mix hex.owner add PACKAGE EMAIL_OR_USERNAME
        mix hex.owner transfer PACKAGE EMAIL_OR_USERNAME
        mix hex.owner remove PACKAGE EMAIL_OR_USERNAME
        mix hex.owner list PACKAGE
        mix hex.owner packages
        """)
    end
  end

  @impl true
  def tasks() do
    [
      {"add PACKAGE EMAIL_OR_USERNAME", "Adds an owner to package"},
      {"transfer PACKAGE EMAIL_OR_USERNAME",
       "Transfers ownership of a package to another user or organization"},
      {"remove PACKAGE EMAIL_OR_USERNAME", "Removes an owner from package"},
      {"list PACKAGE", "List all owners of a given package"},
      {"packages", "List all packages owned by the current user"}
    ]
  end

  defp add_owner(organization, package, owner, level) when level in ~w[full maintainer] do
    auth = Mix.Tasks.Hex.auth_info(:write)
    Hex.Shell.info("Adding owner #{owner} with ownership level #{level} to #{package}")

    case Hex.API.Package.Owner.add(organization, package, owner, level, false, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Adding owner failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp add_owner(_organization, _package, _owner, _level) do
    Mix.raise("Invalid ownership level, expected one of: full, maintainer")
  end

  defp transfer_owner(organization, package, owner) do
    auth = Mix.Tasks.Hex.auth_info(:write)
    Hex.Shell.info("Transferring ownership to #{owner} for #{package}")

    case Hex.API.Package.Owner.add(organization, package, owner, "full", true, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Transferring ownership failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp remove_owner(organization, package, owner) do
    auth = Mix.Tasks.Hex.auth_info(:write)
    Hex.Shell.info("Removing owner #{owner} from #{package}")

    case Hex.API.Package.Owner.delete(organization, package, owner, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Removing owner failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp list_owners(organization, package) do
    auth = Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.Package.Owner.get(organization, package, auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        header = ["Email", "Level"]
        owners = Enum.map(body, &[&1["email"], &1["level"]])
        Mix.Tasks.Hex.print_table(header, owners)

      other ->
        Hex.Shell.error("Package owner fetching failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp list_owned_packages() do
    auth = Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.User.me(auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        Enum.each(body["packages"], fn package ->
          name = package_name(package["repository"], package["name"])
          Hex.Shell.info("#{name} - #{package["html_url"]}")
        end)

      other ->
        Hex.Shell.error("Listing owned packages failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp package_name("hexpm", package_name), do: package_name
  defp package_name(repository_name, package_name), do: repository_name <> "/" <> package_name
end
