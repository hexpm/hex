defmodule Mix.Tasks.Hex.Repo do
  use Mix.Task

  @shortdoc "Manages Hex repositories"

  @moduledoc """
  Manages the list of available Hex repositories.

  The repository is where packages and the registry of packages is stored.
  You can fetch packages from multiple different repositories and packages
  can depend on packages from other repositories. To use a package from another
  repository than the global default `hexpm` add `repo: "my_repo"` to the
  dependency declaration in `mix.exs`:

      {:plug, "~> 1.0", repo: "my_repo"}

  By default all dependencies of plug will also be fetched from `my_repo`
  unless plug has declared otherwise in its dependency definition.

  To use packages from `my_repo` you need to add it to your configuration
  first. You do that by calling `mix hex.repo add my_repo https://myrepo.example.com`.

  The default repo is called `hexpm` and points to https://repo.hex.pm. This
  can be overridden by using `mix hex.repo set ...`.

  A repository configured from an organization will have `hexpm:` prefixed to
  its name. To depend on packages from an organization add `repo: "hexpm:my_organization"`
  to the dependency declaration or simply `organization: "my_organization"`.

  To configure organizations, see the `hex.organization` task.

  ## Add a repo

      mix hex.repo add NAME URL

  ### Command line options

    * `--public-key PATH` - Path to public key used to verify the registry (optional).

    * `--auth-key KEY` - Key used to authenticate HTTP requests to repository (optional).

    * `--fetch-public-key FINGERPRINT` - Download public key from the repository and verify against the fingerprint (optional).

  ## Set config for repo

      mix hex.repo set NAME --url URL
      mix hex.repo set NAME --public-key PATH
      mix hex.repo set NAME --auth-key KEY

  ## Get config for repo

      mix hex.repo get NAME --url
      mix hex.repo get NAME --public-key
      mix hex.repo get NAME --auth-key

  ## Remove repo

      mix hex.repo remove NAME

  ## Show repo config

      mix hex.repo show NAME

  ## List all repos

      mix hex.repo list
  """
  @behaviour Hex.Mix.TaskDescription

  @add_switches [public_key: :string, auth_key: :string, fetch_public_key: :string]
  @set_switches [url: :string, public_key: :string, auth_key: :string]
  @show_switches [url: :boolean, public_key: :boolean, auth_key: :boolean]

  @impl true
  def run(all_args) do
    Hex.start()
    {_opts, args} = Hex.OptionParser.parse!(all_args, switches: [])

    case args do
      ["add", name, url] ->
        {opts, _args} = Hex.OptionParser.parse!(all_args, strict: @add_switches)
        add(name, url, opts)

      ["set", name] ->
        {opts, _args} = Hex.OptionParser.parse!(all_args, strict: @set_switches)
        set(name, opts)

      ["remove", name] ->
        remove(name)

      ["show", name] ->
        {opts, _args} = Hex.OptionParser.parse!(all_args, strict: @show_switches)
        show(name, opts)

      ["list"] ->
        list()

      _ ->
        invalid_args()
    end
  end

  defp invalid_args() do
    Mix.raise("""
    Invalid arguments, expected one of:

    mix hex.repo add NAME URL
    mix hex.repo set NAME
    mix hex.repo get NAME
    mix hex.repo remove NAME
    mix hex.repo show NAME
    mix hex.repo list
    """)
  end

  @impl true
  def tasks() do
    [
      {"add NAME URL", "Add a repo"},
      {"set NAME", "Set config for repo"},
      {"get NAME", "Get config for repo"},
      {"remove NAME", "Remove repo"},
      {"list", "List all repos"}
    ]
  end

  defp add(name, url, opts) do
    public_key =
      read_public_key(opts[:public_key]) ||
        fetch_public_key(opts[:fetch_public_key], url, opts[:auth_key])

    repo =
      %{
        url: url,
        public_key: nil,
        fetch_public_key: nil,
        auth_key: nil
      }
      |> Map.merge(Enum.into(opts, %{}))
      |> Map.put(:public_key, public_key)

    Hex.State.fetch!(:repos)
    |> Map.put(name, repo)
    |> Hex.Config.update_repos()
  end

  defp set(name, opts) do
    opts =
      if public_key = opts[:public_key] do
        Keyword.put(opts, :public_key, read_public_key(public_key))
      else
        opts
      end

    Hex.State.fetch!(:repos)
    |> Map.update!(name, &Map.merge(&1, Enum.into(opts, %{})))
    |> Hex.Config.update_repos()
  end

  defp remove(name) do
    Hex.State.fetch!(:repos)
    |> Map.delete(name)
    |> Hex.Config.update_repos()
  end

  defp list() do
    header = ["Name", "URL", "Public key", "Auth key"]

    values =
      Enum.map(Hex.State.fetch!(:repos), fn {name, config} ->
        [
          name,
          config[:url],
          show_public_key(config[:public_key]),
          config[:auth_key]
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
      Mix.raise("""
      Could not decode public key. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format.
      """)
  end

  defp show_public_key(nil), do: nil

  defp show_public_key(public_key) do
    [pem_entry] = :public_key.pem_decode(public_key)
    public_key = :public_key.pem_entry_decode(pem_entry)
    ssh_hostkey_fingerprint(public_key)
  end

  defp fetch_public_key(nil, _, _), do: nil

  defp fetch_public_key(fingerprint, repo_url, auth_key) do
    case Hex.Repo.get_public_key(repo_url, auth_key) do
      {:ok, {200, key, _}} ->
        if show_public_key(key) == fingerprint do
          key
        else
          Mix.raise("Public key fingerprint mismatch")
        end

      {:ok, {code, _, _}} ->
        Hex.Shell.error("Downloading public key failed with code \"#{inspect(code)}\"")
        Mix.Tasks.Hex.set_exit_code(1)

      other ->
        Hex.Shell.error("Downloading public key failed")
        Hex.Utils.print_error_result(other)
        Mix.Tasks.Hex.set_exit_code(1)
    end
  end

  # Adapted from https://github.com/erlang/otp/blob/3eddb0f762de248d3230b38bc9d478bfbc8e7331/lib/public_key/src/public_key.erl#L824
  defp ssh_hostkey_fingerprint(key) do
    "SHA256:#{sshfp_string(key)}"
  end

  defp sshfp_string(key) do
    :crypto.hash(:sha256, Hex.Stdlib.ssh2_pubkey_encode(key))
    |> Hex.Stdlib.base_encode64_nopadding()
  end

  defp show(name, [{key, _} | _]) do
    case Map.fetch(Hex.State.fetch!(:repos), name) do
      {:ok, config} ->
        Hex.Shell.info(Map.get(config, key, ""))

      :error ->
        Mix.raise("Config does not contain repo #{name}")
    end
  end

  defp show(name, []) do
    case Map.fetch(Hex.State.fetch!(:repos), name) do
      {:ok, repo} ->
        header = ["URL", "Public key", "Auth key"]
        rows = [[repo.url, show_public_key(repo.public_key), repo.auth_key]]
        Mix.Tasks.Hex.print_table(header, rows)

      :error ->
        Mix.raise("Config does not contain repo #{name}")
    end
  end
end
