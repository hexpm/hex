defmodule Hex.Repo do
  alias Hex.HTTP

  @public_keys_html "https://hex.pm/docs/public_keys"

  def get_repo(repo) do
    repo = repo || "hexpm"
    repos = Hex.State.fetch!(:repos)
    case Map.fetch(repos, repo) do
      {:ok, config} ->
        mirror_url = Hex.State.fetch!(:mirror_url)
        if repo == "hexpm" and mirror_url do
          Map.put(config, :url, mirror_url)
        else
          config
        end
      :error ->
        Mix.raise "Unknown repository #{inspect repo}, add new repositories " <>
                  "with the `mix hex.repo` or `mix hex.organization` tasks"
    end
  end

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
    %{signature: signature, payload: payload} = :hex_pb_signed.decode_msg(body, :Signed)
    public_key = get_repo(repo).public_key
    if public_key && Hex.State.fetch!(:check_registry?) do
      do_verify(payload, signature, public_key, repo)
    end
    payload
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
    elixir_version = Hex.Version.parse!(System.version)

    entries
    |> Enum.reverse()
    |> Enum.find_value(&find_version(&1, elixir_version))
  end

  defp find_version([hex_version, _digest | compatible_versions], elixir_version) do
    if Enum.find(compatible_versions, &Hex.Version.compare(&1, elixir_version) != :gt) do
      hex_version
    end
  end

  # Treat missing as latest
  defp is_version_newer(nil), do: :latest
  defp is_version_newer(hex_version) do
    if Hex.Version.compare(hex_version, Hex.version) == :gt do
      {:version, hex_version}
    else
      :latest
    end
  end

  defp do_verify(payload, signature, public_key, repo) do
    unless public_key do
      Mix.raise "No public key stored for #{repo}. Either install a public " <>
                "key with `mix hex.public_keys` or disable the registry " <>
                "verification check by setting `HEX_UNSAFE_REGISTRY=1`."
    end

    unless Hex.Crypto.PublicKey.verify(payload, :sha512, signature, [public_key], repo) do
      Mix.raise "Could not verify authenticity of fetched registry file. " <>
                "This may happen because a proxy or some entity is " <>
                "interfering with the download or because you don't have a " <>
                "public key to verify the registry.\n\nYou may try again " <>
                "later or check if a new public key has been released on " <>
                "our public keys page: #{@public_keys_html}"
    end
  end

  def decode(body) do
    %{releases: releases} = :hex_pb_package.decode_msg(body, :Package)
    releases
  end
end
