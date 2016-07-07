defmodule Hex.Crypto.AES_GCM do
  @moduledoc ~S"""
  Content Encryption with AES GCM

  See: https://tools.ietf.org/html/rfc7518#section-5.3
  See: http://csrc.nist.gov/publications/nistpubs/800-38D/SP-800-38D.pdf
  """

  @spec content_encrypt({binary, binary}, <<_::16>> | <<_::24>> | <<_::32>>, <<_::12>>) :: {binary, binary}
  def content_encrypt({aad, plain_text}, key, iv)
      when is_binary(aad)
      and is_binary(plain_text)
      and bit_size(key) in [128, 192, 256]
      and bit_size(iv) === 96 do
    :crypto.block_encrypt(:aes_gcm, key, iv, {aad, plain_text})
  end

  @spec content_decrypt({binary, binary, binary}, <<_::16>> | <<_::24>> | <<_::32>>, <<_::12>>) :: {:ok, binary} | :error
  def content_decrypt({aad, cipher_text, cipher_tag}, key, iv)
      when is_binary(aad)
      and is_binary(cipher_text)
      and bit_size(cipher_tag) === 128
      and bit_size(key) in [128, 192, 256]
      and bit_size(iv) === 96 do
    case :crypto.block_decrypt(:aes_gcm, key, iv, {aad, cipher_text, cipher_tag}) do
      plain_text when is_binary(plain_text) ->
        {:ok, plain_text}
      _ ->
        :error
    end
  end

  ## Content Encryptor

  @behaviour Hex.Crypto.ContentEncryptor

  def init(options) do
    case Keyword.fetch(options, :key) do
      {:ok, key} when is_binary(key) and bit_size(key) in [128, 192, 256] ->
        case Keyword.fetch(options, :iv) do
          {:ok, iv} when is_binary(iv) and bit_size(iv) === 96 ->
            params = %{
              key: key,
              iv: iv
            }
            {:ok, params}
          _ ->
            {:error, ":iv is required, must be a bitstring, and must be 96 bits"}
        end
      _ ->
        {:error, ":key is required, must be a bitstring, and must be 128, 192, or 256 bits"}
    end
  end

  def encrypt({aad, plain_text}, %{key: key, iv: iv}) do
    content_encrypt({aad, plain_text}, key, iv)
  end

  def decrypt({aad, cipher_text, cipher_tag}, %{key: key, iv: iv}) do
    content_decrypt({aad, cipher_text, cipher_tag}, key, iv)
  end

  def encode(%{key: key}) do
    algorithm =
      case bit_size(key) do
        128 -> "A128GCM"
        192 -> "A192GCM"
        256 -> "A256GCM"
      end
    {algorithm, <<>>}
  end

  def decode(algorithm, <<>>, options) when algorithm in ["A128GCM", "A192GCM", "A256GCM"] do
    key_length =
      case algorithm do
        "A128GCM" -> 128
        "A192GCM" -> 192
        "A256GCM" -> 256
      end
    case init(options) do
      {:ok, params = %{key: key}} when bit_size(key) === key_length ->
        {:ok, params}
      {:ok, _} ->
        :error
      init_error ->
        init_error
    end
  end
  def decode(_, _, _),
    do: :error

end