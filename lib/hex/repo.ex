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

  # Responses from a repository, which can be an untrusted mirror, are limited
  # to the largest file Hex downloads from one, a package or docs tarball,
  # which hex_core limits to 16 MiB with tarball_max_size and
  # docs_tarball_max_size
  @max_body_size 16 * 1024 * 1024

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

  defp default_organization(repo, origin, name) do
    source = origin.source
    public_key = merge_values(Map.get(repo, :public_key), source.public_key)
    auth_key = merge_values(Map.get(repo, :auth_key), source.auth_key)

    oauth_exchange =
      merge_values(Map.get(repo, :oauth_exchange), Map.get(source, :oauth_exchange))

    oauth_exchange_url =
      merge_values(
        Map.get(repo, :oauth_exchange_url, :undefined),
        Map.get(source, :oauth_exchange_url, :undefined)
      )

    {repo, trusted} = put_organization_url(repo, origin, name)

    repo
    |> Map.put(:public_key, public_key)
    |> Map.put(:auth_key, auth_key)
    |> Map.put(:oauth_exchange, oauth_exchange)
    |> Map.put(:oauth_exchange_url, oauth_exchange_url)
    |> Map.put(:trusted, trusted)
  end

  # build_url appends "/repos/<org>" via :repo_organization. A missing URL, or
  # the URL of a repository the organization's is derived from, with or
  # without "/repos/<org>", becomes the derived URL: a repository's own URL
  # isn't an organization's custom URL, it's the repository itself. For a
  # Hex.pm organization, any other repo.hex.pm URL stays on repo.hex.pm. Any
  # other URL is a custom URL. Returns the repository and whether it's trusted.
  defp put_organization_url(repo, origin, name) do
    url = Map.get(repo, :url)

    cond do
      url == nil or url_in?(url, origin.derived_urls, name) ->
        repo = Map.merge(repo, %{url: origin.source.url, repo_organization: name})
        {repo, origin.source.trusted}

      origin.hexpm? and url_in?(url, [@hexpm_url], name) ->
        {Map.merge(repo, %{url: @hexpm_url, repo_organization: name}), true}

      true ->
        {Map.delete(repo, :repo_organization), origin.custom_trusted}
    end
  end

  defp url_in?(url, urls, name) do
    url = trim_url(url)
    Enum.any?(urls, &(url in [trim_url(&1), trim_url(&1) <> "/repos/#{name}"]))
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
      [source_name, organization] ->
        origin = organization_source(source_name, Hex.State.fetch!(:repos), nil)
        {:ok, default_organization(%{}, origin, organization)}

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

  def merge_hexpm(repos, hexpm) do
    Map.update(repos, "hexpm", hexpm, &Map.merge(hexpm, &1))
  end

  # Hex.State.init passes the mirror URLs, since it loads the repos in the
  # Hex.State process. Everywhere else they're read from Hex.State.
  def update_organizations(repos, mirrors \\ nil) do
    Map.new(repos, fn {name, repo} ->
      case split_repo_name(name) do
        [source_name, organization] ->
          origin = organization_source(source_name, repos, mirrors)
          {name, default_organization(repo, origin, organization)}

        _ ->
          {name, Map.put(repo, :trusted, true)}
      end
    end)
  end

  def clean_organizations(repos) do
    Map.new(repos, fn {name, repo} ->
      case split_repo_name(name) do
        [source_name, organization] ->
          origin = organization_source(source_name, repos, nil)
          default = default_organization(%{}, origin, organization)
          {name, clean_repo(default_organization(repo, origin, organization), default)}

        _ ->
          {name, Map.delete(repo, :trusted)}
      end
    end)
  end

  # Returns the repository an organization's repository takes its config from,
  # the repository URLs its URL is derived from, whether it's a Hex.pm
  # organization, and whether a custom URL is trusted. In the Hex.State
  # process, while Hex.State.init loads the repos, and without Hex.State, only
  # the given repos and mirror URLs are available. Elsewhere the hexpm
  # repository's URL in Hex.State is included, since the given repos can
  # change it.
  defp organization_source(source_name, repos, mirrors) do
    state_pid = Process.whereis(Hex.State)

    if state_pid && state_pid != self() do
      state_hexpm = Map.fetch!(Hex.State.fetch!(:repos), "hexpm")
      hexpm_urls = [Map.get(repos, "hexpm", state_hexpm).url, state_hexpm.url]

      mirrors = %{
        mirror_url: Hex.State.fetch!(:mirror_url),
        trusted_mirror_url: Hex.State.fetch!(:trusted_mirror_url)
      }

      organization_source(source_name, get_repo(source_name), hexpm_urls, mirrors)
    else
      hexpm_urls = if hexpm = repos["hexpm"], do: [hexpm.url], else: []
      mirrors = mirrors || %{mirror_url: nil, trusted_mirror_url: nil}
      organization_source(source_name, Map.fetch!(repos, source_name), hexpm_urls, mirrors)
    end
  end

  # A Hex.pm organization's repository goes through a mirror only when it's set
  # with HEX_TRUSTED_MIRROR. A mirror set with HEX_MIRROR doesn't get
  # credentials, so it can't serve the repository, and it's fetched from the
  # hexpm repository's own URL instead, which isn't trusted either when it's
  # the HEX_MIRROR URL. A custom URL is trusted when the hexpm repository is.
  defp organization_source("hexpm", source, [hexpm_url | _] = hexpm_urls, mirrors) do
    %{mirror_url: mirror_url, trusted_mirror_url: trusted_mirror_url} = mirrors
    url = trusted_mirror_url || hexpm_url

    trusted =
      trusted_mirror_url != nil or mirror_url == nil or trim_url(url) != trim_url(mirror_url)

    %{
      source: %{source | url: url, trusted: trusted},
      derived_urls: Enum.reject([url, mirror_url | hexpm_urls], &is_nil/1),
      hexpm?: true,
      custom_trusted: trusted_mirror_url != nil or mirror_url == nil
    }
  end

  # Repositories other than hexpm are always trusted. The config they're
  # loaded from in Hex.State.init doesn't store it.
  defp organization_source(_source_name, source, _hexpm_urls, _mirrors) do
    trusted = Map.get(source, :trusted, true)

    %{
      source: Map.put(source, :trusted, trusted),
      derived_urls: [source.url],
      hexpm?: false,
      custom_trusted: trusted
    }
  end

  defp trim_url(url), do: String.trim_trailing(url, "/")

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
        if :mix_hex_cli_auth.is_token_expired(expires_at) do
          Map.delete(repo, :oauth_token)
        else
          repo
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

  def get_tarball_to_file(repo, package, version, filename) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo)

    Hex.Auth.with_repo(
      config,
      &:mix_hex_repo.get_tarball_to_file(&1, package, version, filename),
      optional: true
    )
  end

  # Resolves the repository's credentials without making a request, so one
  # that has to be exchanged is exchanged once, ahead of the parallel fetches
  # that use it.
  def resolve_auth(repo) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo)
    :mix_hex_cli_auth.resolve_repo_auth(config)
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
      | http_adapter: {Hex.HTTP, %{max_body_size: @max_body_size}},
        repo_name: repo_name,
        repo_organization: Map.get(repo_config, :repo_organization, :undefined),
        repo_url: repo_config.url,
        repo_public_key: Map.get(repo_config, :public_key),
        repo_verify: !unsafe_registry,
        repo_verify_origin: !no_verify_repo_origin,
        trusted: Map.get(repo_config, :trusted, false),
        oauth_exchange: Map.get(repo_config, :oauth_exchange, false)
    }

    config =
      case Map.fetch(repo_config, :oauth_exchange_url) do
        {:ok, oauth_exchange_url} -> Map.put(config, :oauth_exchange_url, oauth_exchange_url)
        :error -> Map.put(config, :oauth_exchange_url, config.api_url)
      end

    config =
      if repo_name == "hexpm" and is_binary(organization) do
        Hex.Auth.put_organization_auth(config, organization, repo_config)
      else
        config
      end

    if etag do
      %{config | http_etag: etag}
    else
      config
    end
  end
end
