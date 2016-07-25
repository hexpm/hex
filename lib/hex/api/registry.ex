defmodule Hex.API.Registry do
  alias Hex.API
  alias Hex.Crypto.PublicKey

  @default_repo "https://repo.hex.pm"
  @public_keys_html "https://hex.pm/docs/public_keys"

  def get_package(name, opts \\ []) do
    headers =
      if etag = opts[:etag] do
        %{'if-none-match' => etag}
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
