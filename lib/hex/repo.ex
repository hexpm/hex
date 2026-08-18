defmodule Hex.Repo do
  @moduledoc false

  @hexpm_url "https://repo.hex.pm"
  @hexpm_public_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
  Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
  IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
  3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
  XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
  J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
  0wIDAQAB
  -----END PUBLIC KEY-----
  """

  def fetch_repo(repo) do
    repo = repo || "hexpm"
    repos = Hex.State.fetch!(:repos)

    case Map.fetch(repos, repo) do
      {:ok, config} when repo == "hexpm" ->
        hexpm = hexpm_repo()
        url = hexpm.url || config.url
        auth_key = hexpm.auth_key || config.auth_key
        {:ok, %{config | url: url, trusted: hexpm.trusted, auth_key: auth_key}}

      {:ok, config} ->
        {:ok, config}

      :error ->
        fetch_organization_fallback(repo)
    end
  end

  def get_repo(repo) do
    case fetch_repo(repo) do
      {:ok, config} ->
        config

      :error ->
        unknown_repo_error(repo)
    end
  end

  defp default_organization(repo, source, name) do
    public_key = merge_values(Map.get(repo, :public_key), source.public_key)
    auth_key = merge_values(Map.get(repo, :auth_key), source.auth_key)

    oauth_exchange =
      merge_values(Map.get(repo, :oauth_exchange), Map.get(source, :oauth_exchange))

    oauth_exchange_url =
      merge_values(
        Map.get(repo, :oauth_exchange_url, :undefined),
        Map.get(source, :oauth_exchange_url, :undefined)
      )

    repo
    |> put_organization_url(source, name)
    |> Map.put(:public_key, public_key)
    |> Map.put(:auth_key, auth_key)
    |> Map.put(:oauth_exchange, oauth_exchange)
    |> Map.put(:oauth_exchange_url, oauth_exchange_url)
    |> Map.put(:trusted, Map.has_key?(repo, :auth_key) or source.trusted)
  end

  # build_url appends "/repos/<org>" via :repo_organization. Normalize a missing
  # or baked-in "/repos/<org>" URL to that form; keep a custom URL as-is.
  defp put_organization_url(repo, source, name) do
    url = Map.get(repo, :url)

    if url in [nil, source.url <> "/repos/#{name}"] do
      repo
      |> Map.put(:url, source.url)
      |> Map.put(:repo_organization, name)
    else
      repo
    end
  end

  def hexpm_repo() do
    trusted_mirror_url = Hex.State.fetch!(:trusted_mirror_url)
    mirror_url = Hex.State.fetch!(:mirror_url)
    auth_key = Hex.State.fetch!(:repos_key)

    %{
      url: trusted_mirror_url || mirror_url,
      public_key: @hexpm_public_key,
      auth_key: auth_key,
      oauth_exchange: true,
      trusted: trusted_mirror_url != nil or mirror_url == nil
    }
  end

  def default_hexpm_repo() do
    %{
      url: @hexpm_url,
      public_key: @hexpm_public_key,
      auth_key: nil,
      oauth_exchange: true,
      trusted: true
    }
  end

  defp fetch_organization_fallback(repo) do
    case String.split(repo, ":", parts: 2) do
      [source, organization] ->
        source = get_repo(source)
        {:ok, default_organization(%{}, source, organization)}

      _ ->
        :error
    end
  end

  defp unknown_repo_error("hexpm:" <> organization) do
    Mix.raise(
      "Unknown organization #{inspect(organization)}, authorize with `mix hex.user auth` " <>
        "or add new organizations with the `mix hex.organization auth` task"
    )
  end

  defp unknown_repo_error(repo) do
    Mix.raise(
      "Unknown repository #{inspect(repo)}, add new repositories " <>
        "with the `mix hex.repo add` task"
    )
  end

  def merge_hexpm(repos, hexpm \\ hexpm_repo()) do
    Map.update(repos, "hexpm", hexpm, &Map.merge(hexpm, &1))
  end

  def update_organizations(repos) do
    Map.new(repos, fn {name, repo} ->
      case split_repo_name(name) do
        [source, organization] ->
          state_pid = Process.whereis(Hex.State)

          source =
            if state_pid && state_pid != self() do
              get_repo(source)
            else
              Map.fetch!(repos, source)
            end

          repo = default_organization(repo, source, organization)
          {name, repo}

        _ ->
          {name, Map.put(repo, :trusted, true)}
      end
    end)
  end

  def clean_organizations(repos) do
    Map.new(repos, fn {name, repo} ->
      case split_repo_name(name) do
        [source, organization] ->
          source = get_repo(source)
          default = default_organization(%{}, source, organization)
          {name, clean_repo(repo, default)}

        _ ->
          {name, Map.delete(repo, :trusted)}
      end
    end)
  end

  def clean_hexpm(repos) do
    hexpm = hexpm_repo()
    repo = Map.get(repos, "hexpm", hexpm)
    repo = clean_repo(repo, hexpm)

    if repo == %{} do
      Map.delete(repos, "hexpm")
    else
      Map.put(repos, "hexpm", repo)
    end
  end

  defp clean_repo(repo, default) do
    repo
    |> clean_expired_oauth_token()
    |> Map.delete(:trusted)
    |> Enum.reject(fn {key, value} -> value in [nil, Map.get(default, key)] end)
    |> Map.new()
  end

  defp clean_expired_oauth_token(repo) do
    case repo[:oauth_token] do
      %{expires_at: expires_at} ->
        current_time = System.system_time(:second)

        # Keep token if it's valid for more than 5 minutes (300 seconds)
        if expires_at > current_time + 300 do
          repo
        else
          Map.delete(repo, :oauth_token)
        end

      _ ->
        repo
    end
  end

  defp merge_values(nil, right), do: right
  defp merge_values(:undefined, right), do: right
  defp merge_values(left, _right), do: left

  def get_package(repo, package, etag) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo, etag)

    Hex.Auth.with_repo(config, &:mix_hex_repo.get_package(&1, package), optional: true)
  end

  def get_policy(repo, name, etag) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo, etag)

    Hex.Auth.with_repo(config, &:mix_hex_repo.get_policy(&1, name), optional: true)
  end

  def get_docs(repo, package, version) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo)

    Hex.Auth.with_repo(config, &:mix_hex_repo.get_docs(&1, package, version), optional: true)
  end

  def get_tarball(repo, package, version) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo)

    Hex.Auth.with_repo(config, &:mix_hex_repo.get_tarball(&1, package, version), optional: true)
  end

  def get_public_key(repo_config) when is_map(repo_config) do
    config = build_hex_core_config(repo_config, "")

    Hex.Auth.with_preemptive_auth(repo_config, config, &:mix_hex_repo.get_public_key/1,
      auth_inline: false,
      optional: true
    )
  end

  def get_installs() do
    repo = get_repo("hexpm")
    config = build_hex_core_config(repo, "")

    Hex.Auth.with_repo(config, &:mix_hex_repo.get_hex_installs/1)
  end

  def find_new_version_from_csv(body) do
    body
    |> parse_csv()
    |> find_latest_eligible_version()
    |> version_latest()
  end

  def tarball_url(repo, package, version) do
    config = get_repo(repo)
    config.url <> "/tarballs/#{URI.encode(package)}-#{URI.encode(version)}.tar"
  end

  defp parse_csv(body) do
    body
    |> :binary.split("\n", [:global, :trim])
    |> Enum.map(&:binary.split(&1, ",", [:global, :trim]))
  end

  defp find_latest_eligible_version(entries) do
    elixir_version = Version.parse!(System.version())

    entries
    |> Enum.reverse()
    |> Enum.find_value(&find_version(&1, elixir_version))
  end

  defp find_version([hex_version, _digest | compatible_versions], elixir_version) do
    if Enum.find(compatible_versions, &(Version.compare(&1, elixir_version) != :gt)) do
      hex_version
    end
  end

  # Treat missing as latest
  defp version_latest(nil), do: :latest

  defp version_latest(hex_version) do
    if Version.compare(hex_version, Hex.version()) == :gt do
      {:version, hex_version}
    else
      :latest
    end
  end

  defp split_repo_name(name) do
    String.split(name, ":", parts: 2)
  end

  defp build_hex_core_config(repo_config, repo_name, etag \\ nil) do
    unsafe_registry = Hex.State.fetch!(:unsafe_registry)
    no_verify_repo_origin = Hex.State.fetch!(:no_verify_repo_origin)

    {repo_name, organization} =
      case split_repo_name(repo_name) do
        [source, organization] -> {source, organization}
        [name] -> {name, nil}
      end

    config = %{
      Hex.API.Client.config()
      | repo_name: repo_name,
        repo_organization: Map.get(repo_config, :repo_organization, :undefined),
        repo_url: repo_config.url,
        repo_public_key: Map.get(repo_config, :public_key),
        repo_verify: !unsafe_registry,
        repo_verify_origin: !no_verify_repo_origin,
        trusted: Map.get(repo_config, :trusted, false),
        oauth_exchange: Map.get(repo_config, :oauth_exchange, false)
    }

    if repo_config.auth_key do
      maybe_warn_deprecated_repo_key(repo_name, organization, repo_config)
    end

    config =
      case Map.fetch(repo_config, :oauth_exchange_url) do
        {:ok, oauth_exchange_url} -> Map.put(config, :oauth_exchange_url, oauth_exchange_url)
        :error -> Map.put(config, :oauth_exchange_url, config.api_url)
      end

    if etag do
      %{config | http_etag: etag}
    else
      config
    end
  end

  # Only organization repositories are deprecated here. HEX_REPOS_KEY still
  # authenticates the base hexpm repo and trusted mirrors (organization is nil
  # for those), so they must not be warned about.
  defp maybe_warn_deprecated_repo_key("hexpm", organization, repo_config)
       when is_binary(organization) do
    case deprecated_repo_key_source(repo_config) do
      :env ->
        if Hex.Server.should_warn?({:deprecated_repos_key, organization}) do
          Hex.Shell.warn("""
          Authenticating to the #{organization} repository with HEX_REPOS_KEY is deprecated \
          and will stop working in Hex 2.6.

          For development authenticate as a user with `mix hex.user auth`. For CI \
          authenticate per organization with `mix hex.organization auth #{organization} --key KEY`.
          """)
        end

      :config ->
        warn_deprecated_stored_key(organization, Map.get(repo_config, :auth_key_owner))
    end
  end

  defp maybe_warn_deprecated_repo_key(_repo_name, _organization, _repo_config), do: :ok

  defp warn_deprecated_stored_key(_organization, :organization), do: :ok

  defp warn_deprecated_stored_key(organization, :user) do
    if Hex.Server.should_warn?({:deprecated_repo_key, organization}) do
      Hex.Shell.warn("""
      Authenticating to the #{organization} repository with a key owned by your user \
      account is deprecated and will stop working in Hex 2.6.

      For development authenticate as a user with `mix hex.user auth`. For CI generate an \
      organization key with `mix hex.organization key #{organization} generate` and pass it \
      with `mix hex.organization auth #{organization} --key KEY`.
      """)
    end
  end

  defp warn_deprecated_stored_key(organization, _owner) do
    if Hex.Server.should_warn?({:deprecated_repo_key, organization}) do
      Hex.Shell.warn("""
      Authenticating to the #{organization} repository with a stored key is deprecated \
      and will stop working in Hex 2.6.

      For development authenticate as a user with `mix hex.user auth`. For CI generate an \
      organization key with `mix hex.organization key #{organization} generate` and pass it \
      with `mix hex.organization auth #{organization} --key KEY`.

      If you already authenticate with an organization key, re-run \
      `mix hex.organization auth #{organization} --key KEY` on this Hex version to record it \
      and silence this warning.
      """)
    end
  end

  # HEX_REPOS_KEY is exposed as the hexpm source `auth_key` and inherited by
  # `hexpm:*` repos, so an `auth_key` equal to `repos_key` came from the
  # environment, while any other value was stored in the local config.
  defp deprecated_repo_key_source(repo_config) do
    repos_key = Hex.State.fetch!(:repos_key)

    if repos_key != nil and repo_config.auth_key == repos_key do
      :env
    else
      :config
    end
  end
end
