defmodule Hex.API.Registry do
  alias Hex.API
  alias Hex.Crypto.PublicKey

  @default_repo "https://repo.hex.pm"
  @public_keys_html "https://hex.pm/docs/public_keys"

  def get_package(name, opts \\ []) do
    headers =
      if etag = opts[:etag] do
        %{'if-none-match' => Hex.string_to_charlist(etag)}
      end

    API.request(:get, API.repo_url("packages/#{name}"), headers || [])
  end

  def verify(body) do
    %{signature: signature, payload: payload} = :hex_pb_signed.decode_msg(body, :Signed)
    if Hex.State.fetch!(:check_registry?) do
      do_verify(payload, signature)
    end
    payload
  end

  def get_installs do
    API.request(:get, API.repo_url("installs/hex-1.x.csv"), [])
  end

  def find_new_version_from_csv(body) do
    body
    |> parse_csv
    |> find_latest_eligible_version
    |> is_version_newer
  end

  defp parse_csv(body) do
    body
    |> :binary.split("\n", [:global, :trim])
    |> Enum.map(&:binary.split(&1, ",", [:global, :trim]))
  end

  defp find_latest_eligible_version(entries) do
    elixir_version = Hex.Version.parse!(System.version)
    entries
    |> Enum.reverse
    |> Enum.find_value(&find_version(&1, elixir_version))
  end

  defp find_version([hex_version, _digest | compatible_versions], elixir_version) do
    if Enum.find(compatible_versions, &Hex.Version.compare(&1, elixir_version) != :gt) do
      hex_version
    end
  end

  defp is_version_newer(nil), do: nil
  defp is_version_newer(hex_version) do
    if Hex.Version.compare(hex_version, Hex.version) == :gt do
      hex_version
    end
  end

  defp do_verify(payload, signature) do
    repo = Hex.State.fetch!(:repo) || @default_repo
    key = PublicKey.public_key(repo)

    unless key do
      Mix.raise "No public key stored for #{repo}. Either install a public " <>
                "key with `mix hex.public_keys` or disable the registry " <>
                "verification check by setting `HEX_UNSAFE_REGISTRY=1`."
    end

    unless Hex.Crypto.PublicKey.verify(payload, :sha512, signature, [key]) do
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
