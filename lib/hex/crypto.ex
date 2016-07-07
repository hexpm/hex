defmodule Hex.Crypto do

  alias Hex.Crypto.ContentEncryptor
  alias Hex.Crypto.KeyManager

  alias Hex.Crypto.AES_CBC_HMAC_SHA2
  # alias Hex.Crypto.AES_GCM
  alias Hex.Crypto.PBES2_HMAC_SHA2_AES_KW

  def encrypt(plain_text, password, tag \\ "") do
    ## A256GCM with PBES2-HS512+A256KW
    # key = :crypto.strong_rand_bytes(32)
    # iv = :crypto.strong_rand_bytes(12)
    # key_length = byte_size(key)
    # derived_key_length = key_length
    # key_manager = PBES2_HMAC_SHA2_AES_KW
    # content_encryptor = AES_GCM

    ## A256CBC-HS512 with PBES2-HS512+A256KW
    key = :crypto.strong_rand_bytes(64)
    iv = :crypto.strong_rand_bytes(16)
    key_length = byte_size(key)
    derived_key_length = div(key_length, 2)
    key_manager = PBES2_HMAC_SHA2_AES_KW
    content_encryptor = AES_CBC_HMAC_SHA2

    {:ok, key_manager} = KeyManager.init(key_manager, [
      password: password,
      salt_length: 16,
      iterations: Hex.State.fetch!(:pbkdf2_iters),
      derived_key_length: derived_key_length
    ])

    {:ok, content_encryptor} = ContentEncryptor.init(content_encryptor, [
      key: key,
      iv: iv
    ])

    protected = key_manager
    |> KeyManager.encode()
    |> Base.url_encode64(padding: false)
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(ContentEncryptor.encode(content_encryptor), padding: false))
    aad = tag <> protected
    {cipher_text, cipher_tag} = ContentEncryptor.encrypt(content_encryptor, {aad, plain_text})
    encrypted_key = KeyManager.encrypt(key_manager, key)
    encode_encrypted_token(protected, encrypted_key, iv, cipher_text, cipher_tag)
  end

  def decrypt(cipher_text, password, tag \\ "") do
    case decode_encrypted_token(cipher_text) do
      {:ok, {protected, encrypted_key, iv, cipher_text, cipher_tag}} ->
        case String.split(protected, ".", parts: 2) do
          [key_manager, content_encryptor] ->
            case KeyManager.decode(key_manager, [password: password]) do
              {:ok, key_manager} ->
                case KeyManager.decrypt(key_manager, encrypted_key) do
                  {:ok, key} ->
                    case ContentEncryptor.decode(content_encryptor, [key: key, iv: iv]) do
                      {:ok, content_encryptor} ->
                        aad = tag <> protected
                        ContentEncryptor.decrypt(content_encryptor, {aad, cipher_text, cipher_tag})
                      _ ->
                        :error
                    end
                  _ ->
                    :error
                end
              _ ->
                :error
            end
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

  ## Internal

  defp encode_encrypted_token(protected, encrypted_key, iv, cipher_text, cipher_tag) do
    protected
    |> Base.url_encode64(padding: false)
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(encrypted_key, padding: false))
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(iv, padding: false))
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(cipher_text, padding: false))
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(cipher_tag, padding: false))
  end

  defp decode_encrypted_token(token) do
    case String.split(token, ".", parts: 5) do
      [protected, encrypted_key, iv, cipher_text, cipher_tag] ->
        case Base.url_decode64(protected, padding: false) do
          {:ok, protected} ->
            case Base.url_decode64(encrypted_key, padding: false) do
              {:ok, encrypted_key} ->
                case Base.url_decode64(iv, padding: false) do
                  {:ok, iv} ->
                    case Base.url_decode64(cipher_text, padding: false) do
                      {:ok, cipher_text} ->
                        case Base.url_decode64(cipher_tag, padding: false) do
                          {:ok, cipher_tag} ->
                            {:ok, {protected, encrypted_key, iv, cipher_text, cipher_tag}}
                          _ ->
                            :error
                        end
                      _ ->
                        :error
                    end
                  _ ->
                    :error
                end
              _ ->
                :error
            end
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

end
