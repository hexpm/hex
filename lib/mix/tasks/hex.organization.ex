defmodule Mix.Tasks.Hex.Organization do
  use Mix.Task

  @shortdoc "Manages Hex.pm organizations"

  @moduledoc """
  Manages the list of authorized Hex.pm organizations.

  Organizations is a feature of Hex.pm to host and manage private packages. See
  <https://hex.pm/docs/private> for more information.

  By default you will be authorized to all your applications when running
  `mix hex.user auth` and this is the recommended approach. This task is mainly
  provided for a CI and build systems where access to an organization is needed
  without authorizing a user.

  By authorizing a new organization a new key is created for fetching packages
  from the organizations repository and the repository key is stored on the
  local machine.

  To use a package from an organization add `organization: "my_organization"` to the
  dependency declaration in `mix.exs`:

      {:plug, "~> 1.0", organization: "my_organization"}

  ## Authorize an organization

  This command will generate an API key used to authenticate access to the organization.
  See the `hex.user` tasks to list and control all your active API keys.

      mix hex.organization auth ORGANIZATION  [--key KEY] [--key-name KEY_NAME]

  ## Deauthorize and remove an organization

      mix hex.organization deauth NAME

  ## Generate organization key

  This command is useful to pre-generate keys for use with `mix hex.organization auth ORGANIZATION --key KEY`
  on CI servers or similar systems. It returns the hash of the generated key that you can pass to
  `auth ORGANIZATION --key KEY`. Unlike the `hex.user key` commands, a key generated with this
  command is owned by the organization directly, and not the user that generated it. This makes it
  ideal for shared environments such as CI where you don't want to give access to user-specific
  resources and the user's organization membership status won't affect key. By default this command
  sets the `repository` permission which allows read-only access to the repository, it can be
  overriden with the `--permission` flag.

      mix hex.organization key ORGANIZATION --generate [--key-name KEY_NAME] [--permission PERMISSION]

  ## Revoke key

  Removes given key from organization.

  The key can no longer be used to authenticate API requests.

      mix hex.organization key --revoke KEY_NAME

  ## Revoke all keys

  Revoke all keys from the organization.

      mix hex.organization key --revoke-all

  ## List keys

  Lists all keys associated the organization.

      mix hex.organization key --list

  ## List all authorized organizations

  This command will only list organizations you have authorized with this task, it will not
  list organizations you have access to by having authorized with `mix hex.user auth`.

      mix hex.organization list

  ## Command line options

    * `--key KEY` - Hash of key used to authenticate HTTP requests to repository, if
      omitted will generate a new key with your account credentials. This flag
      is useful if you have a key pre-generated with `mix hex.organization key`
      and want to authenticate on a CI server or similar system

    * `--key-name KEY_NAME` - By default Hex will base the key name on your machine's
      hostname and the organization name, use this option to give your own name.

    * `--permission PERMISSION` - Sets the permissions on the key, this option can be given
      multiple times, possibly values are:
      * `api:read` - API read access.
      * `api:write` - API write access.
      * `repository` - Access to the organization's repository.
  """

  @switches [
    revoke_all: :boolean,
    revoke: :string,
    list: :boolean,
    key_name: :string,
    generate: :boolean,
    key: :string,
    permission: [:string, :keep]
  ]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, switches: @switches)

    case args do
      ["auth", name] ->
        auth(name, opts)

      ["deauth", name] ->
        deauth(name)

      ["key", name] ->
        key(name, opts)

      ["list"] ->
        list()

      _ ->
        invalid_args()
    end
  end

  defp invalid_args() do
    Mix.raise("""
    Invalid arguments, expected one of:

    mix hex.organization auth ORGANIZATION
    mix hex.organization deauth ORGANIZATION
    mix hex.organization list
    mix hex.organization key ORGANIZATION --generate
    mix hex.organization key ORGANIZATION --revoke-all
    mix hex.organization key ORGANIZATION --revoke KEY_NAME
    mix hex.organization key ORGANIZATION --list
    """)
  end

  defp auth(organization, opts) do
    key = opts[:key]

    key =
      if key do
        test_key(key, organization)
        key
      else
        key_name = Mix.Tasks.Hex.repository_key_name(organization, opts[:key_name])
        permissions = [%{"domain" => "repository", "resource" => organization}]
        auth = Mix.Tasks.Hex.auth_info(:write)

        case Mix.Tasks.Hex.generate_user_key(key_name, permissions, auth) do
          {:ok, key} -> key
          :error -> nil
        end
      end

    if key do
      Mix.Tasks.Hex.auth_organization("hexpm:#{organization}", key)
    end
  end

  defp deauth(name) do
    Hex.State.fetch!(:repos)
    |> Map.delete("hexpm:#{name}")
    |> Hex.Config.update_repos()
  end

  defp key(name, opts) do
    cond do
      opts[:revoke_all] ->
        revoke_all_keys(name)

      key = opts[:revoke] ->
        revoke_key(name, key)

      opts[:list] ->
        list_keys(name)

      opts[:generate] ->
        generate_unencrypted_key(name, opts)

      true ->
        invalid_args()
    end
  end

  defp revoke_all_keys(organization) do
    auth = Mix.Tasks.Hex.auth_info(:write)

    Hex.Shell.info("Revoking all keys...")

    case Hex.API.Key.Organization.delete_all(organization, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Key revocation failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp revoke_key(organization, key) do
    auth = Mix.Tasks.Hex.auth_info(:write)

    Hex.Shell.info("Revoking key #{key}...")

    case Hex.API.Key.Organization.delete(organization, key, auth) do
      {:ok, {code, _body, _headers}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Key revocation failed")
        Hex.Utils.print_error_result(other)
    end
  end

  # TODO: print permissions
  defp list_keys(organization) do
    auth = Mix.Tasks.Hex.auth_info(:read)

    case Hex.API.Key.Organization.get(organization, auth) do
      {:ok, {code, body, _headers}} when code in 200..299 ->
        values =
          Enum.map(body, fn %{"name" => name, "inserted_at" => time} ->
            [name, time]
          end)

        Mix.Tasks.Hex.print_table(["Name", "Created at"], values)

      other ->
        Hex.Shell.error("Key fetching failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp generate_unencrypted_key(organization, opts) do
    key_name = Mix.Tasks.Hex.general_key_name(opts[:key_name])
    default_permission = [%{"domain" => "repository", "resource" => organization}]
    permissions = Keyword.get_values(opts, :permission)
    permissions = Mix.Tasks.Hex.convert_permissions(permissions) || default_permission

    result =
      Mix.Tasks.Hex.generate_organization_key(
        organization,
        key_name,
        permissions
      )

    case result do
      {:ok, secret} -> Hex.Shell.info(secret)
      :error -> :ok
    end
  end

  defp list() do
    Enum.each(Hex.State.fetch!(:repos), fn {name, _repo} ->
      case String.split(name, ":", parts: 2) do
        ["hexpm", name] ->
          Hex.Shell.info(name)

        _ ->
          :ok
      end
    end)
  end

  defp test_key(key, name) do
    case Hex.API.Auth.get("repository", name, key: key) do
      {:ok, {code, _body, _}} when code in 200..299 ->
        :ok

      other ->
        Hex.Utils.print_error_result(other)
        Mix.raise("Failed to authenticate against organization repository with given key")
    end
  end
end
