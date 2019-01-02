defmodule Hex.Repo do
  @moduledoc false

  alias Hex.HTTP

  @public_keys_html "https://hex.pm/docs/public_keys"
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
      {:ok, config} ->
        mirror_url = Hex.State.fetch!(:mirror_url)

        if repo == "hexpm" and mirror_url do
          {:ok, Map.put(config, :url, mirror_url)}
        else
          {:ok, config}
        end

      :error ->
        fetch_organization_fallback(repo, repos)
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

  def default_organization(source, repo, name) do
    url = merge_values(Map.get(repo, :url), source.url <> "/repos/#{name}")
    public_key = merge_values(Map.get(repo, :public_key), source.public_key)
    auth_key = merge_values(Map.get(repo, :auth_key), source.auth_key)

    repo
    |> Map.put(:url, url)
    |> Map.put(:public_key, public_key)
    |> Map.put(:auth_key, auth_key)
  end

  def default_hexpm_repo(auth_key \\ Hex.State.fetch!(:repos_key)) do
    %{
      url: @hexpm_url,
      public_key: @hexpm_public_key,
      auth_key: auth_key
    }
  end

  defp fetch_organization_fallback(repo, repos) do
    case String.split(repo, ":", parts: 2) do
      [source, organization] ->
        source = Map.fetch!(repos, source)
        {:ok, default_organization(source, %{}, organization)}

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

  def merge_hexpm(repos, hexpm \\ default_hexpm_repo()) do
    Map.update(repos, "hexpm", hexpm, &Map.merge(hexpm, &1))
  end

  def update_organizations(repos) do
    Enum.into(repos, %{}, fn {name, repo} ->
      case String.split(name, ":", parts: 2) do
        [source, organization] ->
          source = Map.fetch!(repos, source)
          repo = default_organization(source, repo, organization)
          {name, repo}

        _ ->
          {name, repo}
      end
    end)
  end

  def clean_organizations(repos) do
    Enum.into(repos, %{}, fn {name, repo} ->
      case String.split(name, ":", parts: 2) do
        [source, organization] ->
          source_repo = Map.fetch!(repos, source)
          repo = put_organization_url(organization, repo, source_repo)
          repo = clean_repo(repo, source_repo)
          {name, repo}

        _ ->
          {name, repo}
      end
    end)
  end

  defp put_organization_url(organization, repo, source_repo) do
    if repo.url == source_repo.url <> "/repos/#{organization}" do
      Map.delete(repo, :url)
    else
      repo
    end
  end

  def clean_hexpm(repos) do
    hexpm = default_hexpm_repo()
    repo = Map.get(repos, "hexpm", hexpm)
    repo = clean_repo(repo, hexpm)

    if repo == %{} do
      Map.delete(repos, "hexpm")
    else
      Map.put(repos, "hexpm", repo)
    end
  end

  defp clean_repo(repo, default) do
    Enum.reduce(default, repo, fn {key, value}, repo ->
      if value == Map.get(repo, key) do
        Map.delete(repo, key)
      else
        repo
      end
    end)
  end

  defp merge_values(nil, right), do: right
  defp merge_values(left, _right), do: left

  def get_package(repo, package, etag) do
    headers = Map.merge(etag_headers(etag), auth_headers(repo))
    HTTP.request(:get, package_url(repo, package), headers, nil)
  end

  def get_docs(repo, package, version) do
    headers = auth_headers(repo)
    HTTP.request(:get, docs_url(repo, package, version), headers, nil)
  end

  def get_tarball(repo, package, version, etag) do
    headers = Map.merge(etag_headers(etag), auth_headers(repo))
    HTTP.request(:get, tarball_url(repo, package, version), headers, nil)
  end

  def verify(body, repo) do
    public_key = get_repo(repo).public_key

    if Hex.State.fetch!(:unsafe_registry) do
      %{payload: payload} = :mix_hex_registry.decode_signed(body)
      payload
    else
      do_verify(body, public_key, repo)
    end
  end

  def get_installs() do
    config = Hex.State.fetch!(:repos)["hexpm"]
    url = config.url <> "/installs/hex-1.x.csv"
    HTTP.request(:get, url, %{}, nil)
  end

  def find_new_version_from_csv(body) do
    body
    |> parse_csv()
    |> find_latest_eligible_version()
    |> is_version_newer()
  end

  defp package_url(repo, package) do
    config = get_repo(repo)
    config.url <> "/packages/#{URI.encode(package)}"
  end

  defp docs_url(repo, package, version) do
    config = get_repo(repo)
    config.url <> "/docs/#{URI.encode(package)}-#{URI.encode(version)}.tar.gz"
  end

  defp tarball_url(repo, package, version) do
    config = get_repo(repo)
    config.url <> "/tarballs/#{URI.encode(package)}-#{URI.encode(version)}.tar"
  end

  defp etag_headers(nil), do: %{}
  defp etag_headers(etag), do: %{'if-none-match' => Hex.string_to_charlist(etag)}

  defp auth_headers(repo) do
    repo = get_repo(repo)

    if key = repo.auth_key do
      %{'authorization' => Hex.string_to_charlist(key)}
    else
      %{}
    end
  end

  defp parse_csv(body) do
    body
    |> :binary.split("\n", [:global, :trim])
    |> Enum.map(&:binary.split(&1, ",", [:global, :trim]))
  end

  defp find_latest_eligible_version(entries) do
    elixir_version = Hex.Version.parse!(System.version())

    entries
    |> Enum.reverse()
    |> Enum.find_value(&find_version(&1, elixir_version))
  end

  defp find_version([hex_version, _digest | compatible_versions], elixir_version) do
    if Enum.find(compatible_versions, &(Hex.Version.compare(&1, elixir_version) != :gt)) do
      hex_version
    end
  end

  # Treat missing as latest
  defp is_version_newer(nil), do: :latest

  defp is_version_newer(hex_version) do
    if Hex.Version.compare(hex_version, Hex.version()) == :gt do
      {:version, hex_version}
    else
      :latest
    end
  end

  defp do_verify(body, public_key, repo) do
    unless public_key do
      Mix.raise(
        "No public key stored for #{repo}. Either install a public " <>
          "key with `mix hex.repo` or disable the registry " <>
          "verification check by setting `HEX_UNSAFE_REGISTRY=1`."
      )
    end

    case :mix_hex_registry.decode_and_verify_signed(body, public_key) do
      {:ok, payload} ->
        payload

      {:error, :unverified} ->
        Mix.raise(
          "Could not verify authenticity of fetched registry file. " <>
            "This may happen because a proxy or some entity is " <>
            "interfering with the download or because you don't have a " <>
            "public key to verify the registry.\n\nYou may try again " <>
            "later or check if a new public key has been released " <> public_key_message(repo)
        )

      {:error, :bad_key} ->
        Mix.raise("invalid public key")
    end
  end

  defp public_key_message("hexpm" <> _), do: "on our public keys page: #{@public_keys_html}"
  defp public_key_message(repo), do: "for repo #{repo}"

  def decode_package(body, repo, package) do
    if Hex.State.fetch!(:no_verify_repo_origin) do
      :mix_hex_pb_package.decode_msg(body, :Package).releases
    else
      case :mix_hex_pb_package.decode_msg(body, :Package) do
        %{releases: releases, repository: ^repo, name: ^package} ->
          releases

        _ ->
          Mix.raise(
            "Fetched deprecated registry record version from repo #{repo}. For security " <>
              "reasons this registry version is no longer supported. The repository " <>
              "you are using should update to fix the security reason. Set " <>
              "HEX_NO_VERIFY_REPO_ORIGIN=1 to disable this check."
          )
      end
    end
  end
end
