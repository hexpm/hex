defmodule Hex.Repo do
  @moduledoc false

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
    repo = get_repo(repo)
    config = build_hex_core_config(repo, etag)
    :mix_hex_repo.get_package(config, package)
  end

  def get_docs(repo, package, version) do
    repo = get_repo(repo)
    config = build_hex_core_config(repo)
    :mix_hex_repo.get_docs(config, package, version)
  end

  def get_tarball(repo, package, version) do
    repo = get_repo(repo)
    config = build_hex_core_config(repo)
    :mix_hex_repo.get_tarball(config, package, version)
  end

  def get_public_key(repo) when is_map(repo) do
    config = build_hex_core_config(repo)
    :mix_hex_repo.get_public_key(config)
  end

  def get_public_key(repo) do
    repo = get_repo(repo)
    config = build_hex_core_config(repo)
    :mix_hex_repo.get_public_key(config)
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
    repo = get_repo("hexpm")
    config = build_hex_core_config(repo)

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

      {:error, :bad_signature} ->
        Mix.raise(
          "Could not verify authenticity of fetched registry file because signature verification failed. " <>
            "This may happen because a proxy or some entity is " <>
            "interfering with the download or because you don't have a " <>
            "public key to verify the registry.\n\nYou may try again " <>
            "later or check if a new public key has been released #{public_key_message(repo)}. " <>
            "Set HEX_UNSAFE_REGISTRY=1 to disable this check and allow insecure package downloads."
        )

      {:error, :bad_repo_name} ->
        Mix.raise(
          "The configured repository name does not match the repository name in the registry. " <>
            "This could be because the repository name is incorrect or " <>
            "because the registry has not been updated to the latest registry format. " <>
            "Set HEX_NO_VERIFY_REPO_ORIGIN=1 to disable this check and allow insecure package downloads."
        )

      {:error, :bad_key} ->
        Mix.raise("invalid public key")
    end
  end

  def public_key_message("hexpm:" <> _), do: "on our public keys page: #{@public_keys_html}"
  def public_key_message("hexpm"), do: "on our public keys page: #{@public_keys_html}"
  def public_key_message(repo), do: "for repo #{repo}"

  def decode_package(body, repo, package) do
    repo = repo_name(repo)

    if Hex.State.fetch!(:no_verify_repo_origin) do
      {:ok, releases} = :mix_hex_registry.decode_package(body, :no_verify, :no_verify)
      releases
    else
      case :mix_hex_registry.decode_package(body, repo, package) do
        {:ok, %{releases: releases}} ->
          outer_checksum? = Enum.all?(releases, &Map.has_key?(&1, :outer_checksum))

          if not outer_checksum? and Hex.Server.should_warn_registry_version?() do
            Hex.Shell.warn(
              "Fetched old registry record version from repo #{repo}. The " <>
                "repository you are using should update to include the new :outer_checksum field"
            )
          end

          releases

        {:error, :bad_repo_name} ->
          Mix.raise(
            "The configured repository name for your dependency #{Hex.Utils.package_name(repo, package)} does not " <>
              "match the repository name in the registry. This could be because the repository name is incorrect or " <>
              "because the registry has not been updated to the latest registry format. " <>
              "Set HEX_NO_VERIFY_REPO_ORIGIN=1 to disable this check and allow insecure package downloads."
          )
      end
    end
  end

  defp split_repo_name(name) do
    String.split(name, ":", parts: 2)
  end

  defp repo_name(name) do
    name |> split_repo_name() |> List.last()
  end

  defp build_hex_core_config(repo, etag \\ nil) do
    config = %{
      :mix_hex_core.default_config()
      | http_adapter: {Hex.HTTP, %{}},
        repo_url: repo.url,
        repo_public_key: Map.get(repo, :public_key),
        repo_verify: true,
        repo_verify_origin: true,
        http_user_agent_fragment: Hex.API.Client.user_agent_fragment()
    }

    config =
      if repo.auth_key && Map.get(repo, :trusted, true) do
        Map.put(config, :repo_key, repo.auth_key)
      else
        config
      end

    if etag do
      Map.put(config, :http_etag, etag)
    else
      config
    end
  end
end
