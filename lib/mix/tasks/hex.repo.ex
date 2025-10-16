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

      $ mix hex.repo add NAME URL

  ### Command line options

    * `--public-key PATH` - Path to public key used to verify the registry (optional).

    * `--auth-key KEY` - Key used to authenticate HTTP requests to repository (optional).

    * `--fetch-public-key FINGERPRINT` - Download public key from the repository and verify against the fingerprint (optional).

    * `--no-oauth-exchange` - Disable OAuth token exchange for API keys. Use the API key directly instead of exchanging it for a short-lived OAuth token (optional).

    * `--oauth-exchange-url URL` - Custom URL for OAuth token exchange. By default, the API URL is used (optional).

  ## Set config for repo

      $ mix hex.repo set NAME --url URL
      $ mix hex.repo set NAME --public-key PATH
      $ mix hex.repo set NAME --auth-key KEY
      $ mix hex.repo set NAME --no-oauth-exchange
      $ mix hex.repo set NAME --oauth-exchange-url URL

  ## Remove repo

      $ mix hex.repo remove NAME

  ## Show repo config

      $ mix hex.repo show NAME
      $ mix hex.repo show NAME --url

  ## List all repos

      $ mix hex.repo list
  """
  @behaviour Hex.Mix.TaskDescription

  @add_switches [
    public_key: :string,
    auth_key: :string,
    fetch_public_key: :string,
    no_oauth_exchange: :boolean,
    oauth_exchange_url: :string
  ]
  @set_switches [
    url: :string,
    public_key: :string,
    auth_key: :string,
    no_oauth_exchange: :boolean,
    oauth_exchange_url: :string
  ]
  @show_switches [
    url: :boolean,
    public_key: :boolean,
    auth_key: :boolean,
    oauth_exchange: :boolean,
    oauth_exchange_url: :boolean
  ]

  @impl true
  def run(all_args) do
    Hex.start()
    {_opts, args} = OptionParser.parse!(all_args, switches: [])

    case args do
      ["add", name, url] ->
        {opts, _args} = OptionParser.parse!(all_args, strict: @add_switches)
        add(name, url, opts)

      ["set", name] ->
        {opts, _args} = OptionParser.parse!(all_args, strict: @set_switches)
        set(name, opts)

      ["remove", name] ->
        remove(name)

      ["show", name] ->
        {opts, _args} = OptionParser.parse!(all_args, strict: @show_switches)
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
      {"remove NAME", "Remove repo"},
      {"show NAME", "Show repo config"},
      {"list", "List all repos"}
    ]
  end

  defp add(name, url, opts) do
    opts_with_exchange = normalize_oauth_exchange_opt(opts)

    public_key =
      read_public_key(opts_with_exchange[:public_key]) ||
        fetch_public_key(
          opts_with_exchange[:fetch_public_key],
          url,
          opts_with_exchange[:auth_key],
          opts_with_exchange[:oauth_exchange]
        )

    repo =
      %{
        url: url,
        public_key: nil,
        fetch_public_key: nil,
        auth_key: nil,
        oauth_exchange: true,
        oauth_exchange_url: nil,
        trusted: true
      }
      |> Map.merge(Map.new(opts_with_exchange))
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

    opts = normalize_oauth_exchange_opt(opts)

    Hex.State.fetch!(:repos)
    |> Map.update!(name, &Map.merge(&1, Map.new(opts)))
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

    Hex.Stdlib.ssh_hostkey_fingerprint(:sha256, public_key)
    |> List.to_string()
  end

  defp fetch_public_key(nil, _, _, _), do: nil

  defp fetch_public_key(fingerprint, repo_url, auth_key, oauth_exchange) do
    repo_config = %{
      url: repo_url,
      auth_key: auth_key,
      trusted: true,
      oauth_exchange: oauth_exchange
    }

    case Hex.Repo.get_public_key(repo_config) do
      {:ok, {200, _, key}} ->
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

  defp show(name, [{key, _} | _]) do
    case Map.fetch(Hex.State.fetch!(:repos), name) do
      {:ok, config} ->
        value = Map.get(config, key, "")
        value = if is_boolean(value), do: to_string(value), else: value
        Hex.Shell.info(value)

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

  defp normalize_oauth_exchange_opt(opts) do
    if Keyword.has_key?(opts, :no_oauth_exchange) do
      oauth_exchange = !opts[:no_oauth_exchange]

      opts
      |> Keyword.delete(:no_oauth_exchange)
      |> Keyword.put(:oauth_exchange, oauth_exchange)
    else
      opts
    end
  end
end
