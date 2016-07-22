defmodule Hex.Crypto.AES_GCM do
  @moduledoc ~S"""
  Content Encryption with AES GCM

  See: https://tools.ietf.org/html/rfc7518#section-5.3
  See: http://csrc.nist.gov/publications/nistpubs/800-38D/SP-800-38D.pdf
  """

  alias Hex.Crypto.ContentEncryptor

  @spec content_encrypt({binary, binary}, <<_::16>> | <<_::24>> | <<_::32>>, <<_::12>>) :: {binary, <<_::16>>}
  def content_encrypt({aad, plain_text}, key, iv)
      when is_binary(aad)
      and is_binary(plain_text)
      and bit_size(key) in [128, 192, 256]
      and bit_size(iv) === 96 do
    :crypto.block_encrypt(:aes_gcm, key, iv, {aad, plain_text})
  end

  @spec content_decrypt({binary, binary, <<_::16>>}, <<_::16>> | <<_::24>> | <<_::32>>, <<_::12>>) :: {:ok, binary} | :error
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

  @behaviour ContentEncryptor

  def init(%{ enc: enc }, _options)
      when enc in ["A128GCM", "A192GCM", "A256GCM"] do
    key_length =
      case enc do
        "A128GCM" -> 16
        "A192GCM" -> 24
        "A256GCM" -> 32
      end
    params = %{
      key_length: key_length
    }
    {:ok, params}
  end

  def encrypt(%{key_length: key_length}, key, iv, {aad, plain_text}) when byte_size(key) == key_length do
    content_encrypt({aad, plain_text}, key, iv)
  end

  def decrypt(%{key_length: key_length}, key, iv, {aad, cipher_text, cipher_tag}) when byte_size(key) == key_length do
    content_decrypt({aad, cipher_text, cipher_tag}, key, iv)
  end

  def generate_key(%{key_length: key_length}) do
    :crypto.strong_rand_bytes(key_length)
  end

  def generate_iv(_params) do
    :crypto.strong_rand_bytes(12)
  end

  def key_length(%{key_length: key_length}) do
    key_length
  end

end