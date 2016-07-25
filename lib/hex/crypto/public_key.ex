defmodule Hex.Crypto.PublicKey do
  @doc """
  Returns the filesystem path for public keys.
  """
  def public_keys_path, do: Path.join(Hex.State.fetch!(:home), "public_keys")

  @doc """
  Returns all public keys as a list.
  """
  def public_keys do
    path = public_keys_path()

    keys = case File.ls(path) do
      {:ok, keys} ->
        Enum.map(keys, &{Base.url_decode64!(&1), File.read!(Path.join(path, &1))})
      {:error, _} ->
        []
    end

    keys ++ [{"https://repo.hex.pm", Hex.State.fetch!(:hexpm_pk)}]
  end

  def public_key(url) do
    Enum.find(public_keys(), fn {url2, _key} -> url == url2 end)
  end

  @doc """
  Decodes a public key and raises if the key is invalid.
  """
  def decode!(id, key) do
    [rsa_public_key] = :public_key.pem_decode(key)
    :public_key.pem_entry_decode(rsa_public_key)
  rescue
    _ ->
      Mix.raise """
      Could not decode public key for #{id}. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format.
      """
  end

  @doc """
  Verifies the given binary has the proper signature using the system public keys.
  """
  def verify(binary, hash, signature, keys) do
    Enum.any?(keys, fn {id, key} ->
      :public_key.verify(binary, hash, signature, decode!(id, key))
    end)
  end
end
