defmodule Hex.Stdlib do
  @moduledoc false

  # TODO: Remove this once we require OTP 24.0
  def ssh_hostkey_fingerprint(digset_type, key) do
    cond do
      # Requires Elixir 1.15.0
      function_exported?(Mix, :ensure_application!, 1) ->
        apply(Mix, :ensure_application!, [:ssh])
        apply(:ssh, :hostkey_fingerprint, [digset_type, key])

      Code.ensure_loaded?(:ssh) and function_exported?(:ssh, :hostkey_fingerprint, 2) ->
        apply(:ssh, :hostkey_fingerprint, [digset_type, key])

      true ->
        apply(:public_key, :ssh_hostkey_fingerprint, [digset_type, key])
    end
  end

  # TODO: Remove this once we require OTP 22.1
  def crypto_hmac(type, key, data) do
    if Code.ensure_loaded?(:crypto) and function_exported?(:crypto, :mac, 4) do
      apply(:crypto, :mac, [:hmac, type, key, data])
    else
      apply(:crypto, :hmac, [type, key, data])
    end
  end

  # TODO: Remove this once we require OTP 22.0
  def crypto_one_time_encrypt(cipher, key, iv, data) do
    if Code.ensure_loaded?(:crypto) and function_exported?(:crypto, :crypto_one_time, 5) do
      apply(:crypto, :crypto_one_time, [cipher, key, iv, data, true])
    else
      apply(:crypto, :block_encrypt, [cipher, key, iv, data])
    end
  end

  # TODO: Remove this once we require OTP 22.0
  def crypto_one_time_decrypt(cipher, key, iv, data) do
    if Code.ensure_loaded?(:crypto) and function_exported?(:crypto, :crypto_one_time, 5) do
      apply(:crypto, :crypto_one_time, [cipher, key, iv, data, false])
    else
      apply(:crypto, :block_decrypt, [cipher, key, iv, data])
    end
  end

  # TODO: Remove this once we require OTP 22.0
  def crypto_one_time_aead_encrypt(cipher, key, iv, plain_text, aad) do
    if Code.ensure_loaded?(:crypto) and function_exported?(:crypto, :crypto_one_time_aead, 5) do
      apply(:crypto, :crypto_one_time_aead, [cipher, key, iv, plain_text, aad, true])
    else
      apply(:crypto, :block_encrypt, [:aes_gcm, key, iv, {aad, plain_text}])
    end
  end

  # TODO: Remove this once we require OTP 22.0
  def crypto_one_time_aead_decrypt(cipher, key, iv, cipher_text, aad, cipher_tag) do
    if Code.ensure_loaded?(:crypto) and function_exported?(:crypto, :crypto_one_time_aead, 5) do
      apply(:crypto, :crypto_one_time_aead, [cipher, key, iv, cipher_text, aad, cipher_tag, false])
    else
      apply(:crypto, :block_decrypt, [:aes_gcm, key, iv, {aad, cipher_text, cipher_tag}])
    end
  end
end
