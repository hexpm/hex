defmodule Mix.Tasks.Hex.Organization do
  use Mix.Task

  @shortdoc "Manages Hex.pm organizations"

  @moduledoc """
  Manages the list of authorized Hex.pm organizations.

  Organizations is a feature of Hex.pm to host and manage private packages. See
  <https://hex.pm/docs/private> for more information.

  By default you will be authorized to all your organizations when running
  `mix hex.user auth` and this is the recommended approach for development. This
  task is mainly provided for CI and build systems where access to an organization
  is needed without authorizing a user.

  For CI, generate an organization key with `mix hex.organization key ORGANIZATION generate`
  and authenticate with it right before fetching dependencies. Hex exchanges the key
  for a short-lived token and stores only the token:

      mix hex.organization auth ORGANIZATION --key KEY

  A GitHub Actions job with the `id-token: write` permission can fetch the
  organization's packages without a key, using Workload Identity, sometimes also
  known as "Trusted Publishing". Configure a workload identity with the `read`
  or `write` role for the organization on hex.pm. When there's no token from
  `mix hex.organization auth --key` and no authenticated user, Hex exchanges the
  job's OIDC token for a short-lived token that can fetch the organization's
  packages.

  To use a package from an organization add `organization: "my_organization"` to the
  dependency declaration in `mix.exs`:

      {:plug, "~> 1.0", organization: "my_organization"}

  ## Authorize an organization

  Exchanges an organization key for a short-lived token used to fetch the
  organization's packages. Hex uses the token for about 25 minutes and doesn't
  refresh it, so in CI run this command right before fetching dependencies. For
  development use `mix hex.user auth` instead.

      $ mix hex.organization auth ORGANIZATION --key KEY

  ## Deauthorize and remove an organization

      $ mix hex.organization deauth NAME

  ## List all authorized organizations

  This command will only list organizations you have authorized with this task, it will not
  list organizations you have access to by having authorized with `mix hex.user auth`.

      $ mix hex.organization list

  ## Generate organization key

  This command is useful to pre-generate keys for use with `mix hex.organization auth ORGANIZATION --key KEY`
  on CI servers or similar systems. It returns the hash of the generated key that you can pass to
  `auth ORGANIZATION --key KEY`. Unlike the `hex.user key` commands, a key generated with this
  command is owned by the organization directly, and not the user that generated it. This makes it
  ideal for shared environments such as CI where you don't want to give access to user-specific
  resources and the user's organization membership status won't affect key. By default this command
  sets the `repository:organization_name` permission which allows read-only access to the organization's repository, it can be
  overridden with the `--permission` flag.

      $ mix hex.organization key ORGANIZATION generate [--key-name KEY_NAME] [--permission PERMISSION]

  ## Revoke key

  Removes given key from organization.

  The key can no longer be used to authenticate API requests.

      $ mix hex.organization key ORGANIZATION revoke KEY_NAME

  ## Revoke all keys

  Revoke all keys from the organization.

      $ mix hex.organization key ORGANIZATION revoke --all

  ## List keys

  Lists all keys associated with the organization.

      $ mix hex.organization key ORGANIZATION list

  ## Command line options

    * `--key KEY` - Required for `auth`. An organization key, generated with
      `mix hex.organization key ORGANIZATION generate`, that is exchanged for a
      short-lived token

    * `--key-name KEY_NAME` - The name of the key created by `key generate`. By
      default Hex bases the key name on your machine's hostname.

    * `--permission PERMISSION` - Sets the permissions on the key, this option can be given
      multiple times, possibly values are:
      * `api:read` - API read access.
      * `api:write` - API write access, which includes every permission below except
        repository access.
      * `repository:organization_name` - Access to the organization's repository (this is the default permission).
      * `publish` - Publish, revert, retire and unretire releases and publish docs of the
        organization's packages.
      * `publish:organization_name/package_name` - The same as `publish`, for one package.
      * `keys` - Create keys with a subset of the key's own permissions, and revoke keys.
      * `owners` - Add and remove package owners.
      * `owners:organization_name/package_name` - The same as `owners`, for one package.
      * `members` - Add and remove members and change their roles.
      * `administration` - Change the organization's number of seats.

      Members can only grant the permissions their organization role allows. Read members
      can grant `api:read` and `repository:organization_name`, write members can also grant
      `publish` and `keys`, and admins can grant everything. When a write member asks for
      `api:write` the key gets `api:read`, `publish` and `keys` instead.
  """
  @behaviour Hex.Mix.TaskDescription

  @switches [
    all: :boolean,
    key_name: :string,
    key: :string,
    permission: [:string, :keep]
  ]

  @impl true
  def run(args) do
    Hex.start()
    {opts, args} = OptionParser.parse!(args, switches: @switches)

    case args do
      ["auth", name] ->
        auth(name, opts)

      ["deauth", name] ->
        deauth(name)

      ["key", name, "generate"] ->
        key_generate(name, opts)

      ["key", name, "revoke", key_name] ->
        key_revoke(name, key_name)

      ["key", name, "revoke"] ->
        if opts[:all], do: key_revoke_all(name), else: invalid_args()

      ["key", name, "list"] ->
        key_list(name)

      ["list"] ->
        list()

      _ ->
        invalid_args()
    end
  end

  @impl true
  def tasks() do
    [
      {"auth ORGANIZATION --key KEY", "Authorize an organization"},
      {"deauth ORGANIZATION", "Deauthorize and remove organization"},
      {"list", "List all authorized organizations"},
      {"key ORGANIZATION generate", "Generate organization key"},
      {"key ORGANIZATION revoke KEY_NAME", "Revoke key"},
      {"key ORGANIZATION revoke --all", "Revoke all keys"},
      {"key ORGANIZATION list", "List keys"}
    ]
  end

  defp invalid_args() do
    Mix.raise("""
    Invalid arguments, expected one of:

    mix hex.organization auth ORGANIZATION --key KEY
    mix hex.organization deauth ORGANIZATION
    mix hex.organization list
    mix hex.organization key ORGANIZATION generate
    mix hex.organization key ORGANIZATION revoke KEY_NAME
    mix hex.organization key ORGANIZATION revoke --all
    mix hex.organization key ORGANIZATION list
    """)
  end

  defp auth(organization, opts) do
    case opts[:key] do
      nil ->
        Mix.raise("""
        Authorizing an organization requires an organization key passed with --key.

        For development authenticate as a user instead, which gives you access to \
        all your organizations:

            mix hex.user auth

        For CI generate an organization key and pass it with --key:

            mix hex.organization key #{organization} generate
            mix hex.organization auth #{organization} --key KEY
        """)

      key ->
        with {:ok, owner} <- test_key(key, organization),
             {:ok, token} <- exchange_key(key, organization) do
          if owner == :user do
            Hex.Shell.warn("""
            The given key is owned by your user account, not the organization. hex.pm \
            will stop accepting user keys for organization repositories.

            For development authenticate as a user with `mix hex.user auth`. For CI generate \
            an organization key with `mix hex.organization key #{organization} generate` and \
            pass it with `--key`.
            """)
          end

          Mix.Tasks.Hex.auth_organization("hexpm:#{organization}", token)
        end
    end
  end

  # Exchanges the key at the organization repository's OAuth exchange URL, or
  # the API URL when it has none.
  defp exchange_key(key, organization) do
    repo = Hex.Repo.get_repo("hexpm:#{organization}")
    config = Hex.API.Client.config()

    config =
      case Map.get(repo, :oauth_exchange_url) do
        url when is_binary(url) -> %{config | api_url: url}
        _ -> config
      end

    client_id = Hex.API.OAuth.client_id()

    case :mix_hex_api_oauth.client_credentials_token(config, client_id, key, "repositories") do
      {:ok, {200, _, %{"access_token" => access_token, "expires_in" => expires_in}}} ->
        expires_at = System.system_time(:second) + expires_in
        {:ok, %{access_token: access_token, expires_at: expires_at}}

      {:ok, {_status, _headers, %{"error" => error} = body}} ->
        description = Hex.Utils.escape_terminal(body["error_description"] || error)
        Hex.Shell.error("Failed to exchange the key for a token: #{description}")
        Mix.Tasks.Hex.set_exit_code(1)
        :error

      other ->
        Hex.Utils.print_error_result(other)
        Hex.Shell.error("Failed to exchange the key for a token")
        Mix.Tasks.Hex.set_exit_code(1)
        :error
    end
  end

  defp deauth(name) do
    Hex.State.fetch!(:repos)
    |> Map.delete("hexpm:#{name}")
    |> Hex.Config.update_repos()
  end

  defp key_revoke_all(organization) do
    Hex.Shell.info("Revoking all keys...")

    case Hex.API.Key.Organization.delete_all(organization) do
      {:ok, {code, _headers, _body}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Key revocation failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp key_revoke(organization, key) do
    Hex.Shell.info("Revoking key #{key}...")

    case Hex.API.Key.Organization.delete(organization, key) do
      {:ok, {code, _headers, _body}} when code in 200..299 ->
        :ok

      other ->
        Hex.Shell.error("Key revocation failed")
        Hex.Utils.print_error_result(other)
    end
  end

  # TODO: print permissions
  defp key_list(organization) do
    case Hex.API.Key.Organization.get(organization) do
      {:ok, {code, _headers, body}} when code in 200..299 ->
        values =
          Enum.map(body, fn %{"name" => name, "inserted_at" => time} ->
            [Hex.Utils.escape_terminal(name), Hex.Utils.escape_terminal(time)]
          end)

        Mix.Tasks.Hex.print_table(["Name", "Created at"], values)

      other ->
        Hex.Shell.error("Key fetching failed")
        Hex.Utils.print_error_result(other)
    end
  end

  defp key_generate(organization, opts) do
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
      {:ok, secret} -> Hex.Shell.info(Hex.Utils.escape_terminal(secret))
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
      {:ok, {code, _, body}} when code in 200..299 ->
        {:ok, key_owner(body)}

      {:ok, {code, _, %{"message" => message}}} when code in [401, 403] ->
        message = Hex.Utils.escape_terminal(message)

        Hex.Shell.error(
          "Failed to authenticate against organization repository with given key because of: #{message}"
        )

        Mix.Tasks.Hex.set_exit_code(1)
        :error

      other ->
        Hex.Utils.print_error_result(other)
        Hex.Shell.error("Failed to verify authentication key")
        Mix.Tasks.Hex.set_exit_code(1)
        :error
    end
  end

  defp key_owner(%{"key" => %{"owner" => %{"type" => "organization"}}}), do: :organization
  defp key_owner(%{"key" => %{"owner" => %{"type" => "user"}}}), do: :user
  defp key_owner(_body), do: nil
end
