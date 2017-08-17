defmodule Mix.Tasks.Hex.Repo do
  use Mix.Task

  @shortdoc "Manages Hex repositories"

  @moduledoc """
  Manages the list of available Hex repositories.

  To use a package from another repository add `repo: :my_repo` to the
  dependency declaration in `mix.exs`:

      {:plug, "~> 1.0", repo: :my_repo}

  By default all dependencies of plug will also be fetched from `:my_repo`
  unless plug has declared otherwise in its dependency definition.

  To use packages from `:my_repo` you need to add it to your configuration
  first. You do that by calling `mix hex.repo my_repo https://myrepo.example.com`.

  The default repo is called `:hexpm` and points to https://repo.hex.pm. This
  can be overridden by using `mix hex.repo set ...`.

  Child dependencies will always be fetched from the same repository as the
  parent package. To override which repository a package is fetched from add
  add the `:repo` option to the package in your dependencies.

  ## Add a repo

      mix hex.repo add NAME URL

  ### Command line options

    * `--public-key path` - Path to public key used to verify the registry (optional).
    * `--auth-key key` - Key used to authenticate HTTP requests to repository (optional).
    * `--api-url url` - This URL will be used when publishing packages to the
      repository when the `:repo` configuration is set in the package configuration.
      It will also be used when passing the `--repo` flag to tasks such as
      `hex.owner` (optional).
    * `--api-key key` - Key used to authenticate requests to the API associated
      with the repository (optional).

  ## Set config for repo

      mix hex.repo set NAME --url URL
      mix hex.repo set NAME --public-key PATH
      mix hex.repo set NAME --auth-key KEY
      mix hex.repo set NAME --api-url URL
      mix hex.repo set NAME --api-key KEY

  ## Remove repo

      mix hex.repo remove NAME

  ## Show repo config

      mix hex.repo show NAME

  ## List all repos

      mix hex.repo list
  """

  @switches [public_key: :string, auth_key: :string, api_url: :string, api_key: :string]

  def run(args) do
    Hex.start()
    {opts, args, _} = OptionParser.parse(args, switches: @switches)

    case args do
      ["add", name, url] ->
        add(name, url, opts)
      ["set", name] ->
        set(name, opts)
      ["remove", name] ->
        remove(name)
      ["show", name] ->
        show(name)
      ["list"] ->
        list()
      _ ->
        Mix.raise """
        Invalid arguments, expected one of:
        mix hex.repo add NAME URL
        mix hex.repo set NAME
        mix hex.repo remove NAME
        mix hex.repo show NAME
        mix hex.repo list
        """
    end
  end

  defp add(name, url, opts) do
    public_key = read_public_key(opts[:public_key])

    repo = %{
      url: url,
      public_key: nil,
      auth_key: nil,
      api_url: nil,
      api_key: nil
    }
    |> Map.merge(Map.new(opts))
    |> Map.put(:public_key, public_key)

    read_config()
    |> Map.put(name, repo)
    |> Hex.Config.update_repos()
  end

  defp set(name, opts) do
    opts = if public_key = opts[:public_key] do
      Keyword.put(opts, :public_key, read_public_key(public_key))
    else
      opts
    end

    read_config()
    |> Map.update!(name, &Map.merge(&1, Map.new(opts)))
    |> Hex.Config.update_repos()
  end

  defp remove(name) do
    read_config()
    |> Map.delete(name)
    |> Hex.Config.update_repos()
  end

  defp list do
    header = ["Name", "URL", "Public key", "Auth key", "API URL", "API key"]
    values =
      Enum.map(read_config(), fn {name, config} ->
        [
          name,
          config[:url],
          show_public_key(config[:public_key]),
          config[:auth_key],
          config[:api_url],
          config[:api_key],
        ]
      end)
    Mix.Tasks.Hex.print_table(header, values)
  end

  defp read_public_key(nil) do
    nil
  end
  defp read_public_key(path) do
    key =
      path
      |> Path.expand()
      |> File.read!()

    decode_public_key(key)
    key
  end

  defp decode_public_key(key) do
    [pem_entry] = :public_key.pem_decode(key)
    :public_key.pem_entry_decode(pem_entry)
  rescue
    _ ->
      Mix.raise """
      Could not decode public key. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format.
      """
  end

  defp read_config() do
    Hex.Config.read()
    |> Hex.Config.read_repos()
  end

  defp show_public_key(nil), do: nil
  defp show_public_key(public_key) do
    [pem_entry] = :public_key.pem_decode(public_key)
    public_key = :public_key.pem_entry_decode(pem_entry)
    ssh_hostkey_fingerprint(public_key)
  end

  # Adapted from https://github.com/erlang/otp/blob/3eddb0f762de248d3230b38bc9d478bfbc8e7331/lib/public_key/src/public_key.erl#L824
  defp ssh_hostkey_fingerprint(key) do
    "SHA256:#{sshfp_string(key)}"
  end

  defp sshfp_string(key) do
    :crypto.hash(:sha256, :public_key.ssh_encode(key, :ssh2_pubkey))
    |> Base.encode64(padding: false)
  end

  defp show(name) do
    repo =
      read_config()
      |> Map.get(name)

    case repo do
      nil ->
        Mix.raise "Config does not contain repo #{name}"
      _ ->
        Mix.Tasks.Hex.print_table(
          ["URL", "Public key", "Auth key"],
          [[repo.url, show_public_key(repo.public_key), repo.auth_key]]
        )
    end
  end
end
