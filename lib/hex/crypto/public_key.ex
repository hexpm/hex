defmodule Hex.Crypto.PublicKey do
  @moduledoc false

  def decode!(id, key) do
    [rsa_public_key] = :public_key.pem_decode(key)
    :public_key.pem_entry_decode(rsa_public_key)
  rescue
    _ ->
      Mix.raise("""
      Could not decode public key for #{id}. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format.
      """)
  end

  def verify(binary, hash, signature, keys, id) do
    Enum.any?(keys, fn key ->
      :public_key.verify(binary, hash, signature, decode!(id, key))
    end)
  end
end
