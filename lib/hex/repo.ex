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
    url = merge_values(Map.get(repo, :url), source.url <> "/repos/#{name}")
    public_key = merge_values(Map.get(repo, :public_key), source.public_key)
    auth_key = merge_values(Map.get(repo, :auth_key), source.auth_key)

    repo
    |> Map.put(:url, url)
    |> Map.put(:public_key, public_key)
    |> Map.put(:auth_key, auth_key)
    |> Map.put(:trusted, Map.has_key?(repo, :auth_key) or source.trusted)
  end

  def hexpm_repo() do
    trusted_mirror_url = Hex.State.fetch!(:trusted_mirror_url)
    mirror_url = Hex.State.fetch!(:mirror_url)
    auth_key = Hex.State.fetch!(:repos_key)

    %{
      url: trusted_mirror_url || mirror_url,
      public_key: @hexpm_public_key,
      auth_key: auth_key,
      trusted: trusted_mirror_url != nil or mirror_url == nil
    }
  end

  def default_hexpm_repo() do
    %{
      url: @hexpm_url,
      public_key: @hexpm_public_key,
      auth_key: nil,
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

          repo =
            repo
            |> put_organization_url(organization, source)
            |> clean_repo(source)

          {name, repo}

        _ ->
          {name, Map.delete(repo, :trusted)}
      end
    end)
  end

  defp put_organization_url(repo, organization, source_repo) do
    if repo.url == source_repo.url <> "/repos/#{organization}" do
      Map.delete(repo, :url)
    else
      repo
    end
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
    |> Map.delete(:trusted)
    |> Enum.reject(fn {key, value} -> value in [nil, Map.get(default, key)] end)
    |> Map.new()
  end

  defp merge_values(nil, right), do: right
  defp merge_values(left, _right), do: left

  def get_package(repo, package, etag) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo, etag)
    :mix_hex_repo.get_package(config, package)
  end

  def get_docs(repo, package, version) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo)
    :mix_hex_repo.get_docs(config, package, version)
  end

  def get_tarball(repo, package, version) do
    repo_config = get_repo(repo)
    config = build_hex_core_config(repo_config, repo)
    :mix_hex_repo.get_tarball(config, package, version)
  end

  def get_public_key(repo_config) when is_map(repo_config) do
    config = build_hex_core_config(repo_config, "")
    :mix_hex_repo.get_public_key(config)
  end

  def get_installs() do
    repo = get_repo("hexpm")
    config = build_hex_core_config(repo, "")

    :mix_hex_repo.get_hex_installs(config)
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
    config = %{
      :mix_hex_core.default_config()
      | http_adapter: {Hex.HTTP, %{}},
        repo_name: hex_to_actual_repo_name(repo_name),
        repo_url: repo_config.url,
        repo_public_key: Map.get(repo_config, :public_key),
        repo_verify: true,
        repo_verify_origin: true,
        http_user_agent_fragment: Hex.API.Client.user_agent_fragment()
    }

    config =
      cond do
        # First priority: explicit repo auth key (from HEX_REPOS_KEY or config)
        repo_config.auth_key && Map.get(repo_config, :trusted, true) ->
          %{config | repo_key: repo_config.auth_key}

        # Second priority: fallback to OAuth token if available
        match?({:ok, _}, Hex.OAuth.get_token()) ->
          {:ok, access_token} = Hex.OAuth.get_token()
          # Format as Bearer token for OAuth authentication
          %{config | repo_key: "Bearer #{access_token}"}

        # No authentication available
        true ->
          config
      end

    if etag do
      %{config | http_etag: etag}
    else
      config
    end
  end

  defp hex_to_actual_repo_name("hexpm:" <> repo), do: repo
  defp hex_to_actual_repo_name(repo), do: repo
end
