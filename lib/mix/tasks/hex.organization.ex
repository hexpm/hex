defmodule Mix.Tasks.Hex.Organization do
  use Mix.Task

  @shortdoc "Manages Hex.pm organizations"

  @moduledoc """
  Manages the list of authorized Hex.pm organizations.

  Organizations is a feature of Hex.pm to host and manage private packages. See
  <https://hex.pm/docs/private> for more information.

  By authorizing a new organization a new key is created for fetching packages
  from the organizations repository and the repository and key is stored on the
  local machine.

  To use a package from an organization add `organization: "my_organization"` to the
  dependency declaration in `mix.exs`:

      {:plug, "~> 1.0", organization: "my_organization"}

  ## Authorize an organization

      mix hex.organization auth NAME

  ### Command line options

    * `--key KEY` - Hash of key used to authenticate HTTP requests to repository, if
      omitted will generate a new key with your account credentials. This flag
      is useful if you have a key pre-generated with `mix hex.organization key`
      and want to authenticate on a CI server or similar system

  ## Deauthorize and remove an organization

      mix hex.organization deauth NAME

  ## Generate a repository autentication key

  This command is useful to pre-generate keys for use with `mix hex.organization auth NAME --key KEY`
  on CI servers or similar systems. It returns the hash of the generated key that you can pass to
  `auth NAME --key KEY`

      mix hex.organization key NAME

  ## List all authorized organizations

      mix hex.organization list
  """

  @switches [key: :string]

  def run(args) do
    Hex.start()
    {opts, args} = Hex.OptionParser.parse!(args, switches: @switches)

    case args do
      ["auth", name] ->
        auth(name, opts)
      ["deauth", name] ->
        deauth(name)
      ["key", name] ->
        key(name)
      ["list"] ->
        list()
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:

        mix hex.organization auth NAME
        mix hex.organization deauth NAME
        mix hex.organization list
        """
    end
  end

  defp auth(name, opts) do
    key = opts[:key]
    if key, do: test_key(key, name)

    hexpm = Hex.Repo.get_repo("hexpm")
    repo = %{
      url: hexpm.url <> "/repos/#{name}",
      public_key: nil,
      auth_key: key || generate_repo_key(name),
    }

    read_config()
    |> Map.put("hexpm:#{name}", repo)
    |> Hex.Config.update_repos()
  end

  defp deauth(name) do
    read_config()
    |> Map.delete("hexpm:#{name}")
    |> Hex.Config.update_repos()
  end

  defp key(name) do
    Hex.Shell.info generate_repo_key(name)
  end

  defp list() do
    Enum.each(read_config(), fn {name, _repo} ->
      case String.split(name, ":", parts: 2) do
        ["hexpm", name] ->
          Hex.Shell.info(name)
        _ ->
          :ok
      end
    end)
  end

  defp read_config() do
    Hex.Config.read()
    |> Hex.Config.read_repos()
  end

  defp generate_repo_key(name) do
    auth = Mix.Tasks.Hex.auth_info()
    permissions = [%{"domain" => "repository", "resource" => name}]

    {:ok, host} = :inet.gethostname()
    key = "#{host}-repository"

    case Hex.API.Key.new(key, permissions, auth) do
      {:ok, {201, body, _}} ->
        body["secret"]
      other ->
        Hex.Utils.print_error_result(other)
        Mix.raise "Generation of repository key failed"
    end
  end

  defp test_key(key, name) do
    case Hex.API.Auth.get("repository", name, [key: key]) do
      {:ok, {code, _body, _}} when code in 200..299 ->
        :ok
      other ->
        Hex.Utils.print_error_result(other)
        Mix.raise "Failed to authenticate against repository with given key"
    end
  end
end
