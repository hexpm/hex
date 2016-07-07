defmodule Hex.Crypto do

  alias Hex.Crypto
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
    |> Crypto.base64url_encode()
    |> Kernel.<>(".")
    |> Kernel.<>(Crypto.base64url_encode(ContentEncryptor.encode(content_encryptor)))
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

  def base64url_encode(binary) do
    try do
      Base.url_encode64(binary, padding: false)
    catch
      _,_ ->
        binary
        |> Base.encode64()
        |> urlsafe_encode64(<<>>)
    end
  end

  def base64url_decode(binary) do
    try do
      Base.url_decode64(binary, padding: false)
    catch
      _,_ ->
        try do
          binary = urlsafe_decode64(binary, <<>>)
          binary =
            case rem(byte_size(binary), 4) do
              2 -> binary <> "=="
              3 -> binary <> "="
              _ -> binary
            end
          Base.decode64(binary)
        catch
          _,_ ->
            :error
        end
    end
  end

  ## Internal

  defp encode_encrypted_token(protected, encrypted_key, iv, cipher_text, cipher_tag) do
    protected
    |> Crypto.base64url_encode()
    |> Kernel.<>(".")
    |> Kernel.<>(Crypto.base64url_encode(encrypted_key))
    |> Kernel.<>(".")
    |> Kernel.<>(Crypto.base64url_encode(iv))
    |> Kernel.<>(".")
    |> Kernel.<>(Crypto.base64url_encode(cipher_text))
    |> Kernel.<>(".")
    |> Kernel.<>(Crypto.base64url_encode(cipher_tag))
  end

  defp decode_encrypted_token(token) do
    case String.split(token, ".", parts: 5) do
      [protected, encrypted_key, iv, cipher_text, cipher_tag] ->
        case Crypto.base64url_decode(protected) do
          {:ok, protected} ->
            case Crypto.base64url_decode(encrypted_key) do
              {:ok, encrypted_key} ->
                case Crypto.base64url_decode(iv) do
                  {:ok, iv} ->
                    case Crypto.base64url_decode(cipher_text) do
                      {:ok, cipher_text} ->
                        case Crypto.base64url_decode(cipher_tag) do
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

  defp urlsafe_encode64(<< ?+, rest :: binary >>, acc),
    do: urlsafe_encode64(rest, << acc :: binary, ?- >>)
  defp urlsafe_encode64(<< ?/, rest :: binary >>, acc),
    do: urlsafe_encode64(rest, << acc :: binary, ?_ >>)
  defp urlsafe_encode64(<< ?=, rest :: binary >>, acc),
    do: urlsafe_encode64(rest, acc)
  defp urlsafe_encode64(<< c, rest :: binary >>, acc),
    do: urlsafe_encode64(rest, << acc :: binary, c >>)
  defp urlsafe_encode64(<<>>, acc),
    do: acc

  defp urlsafe_decode64(<< ?-, rest :: binary >>, acc),
    do: urlsafe_decode64(rest, << acc :: binary, ?+ >>)
  defp urlsafe_decode64(<< ?_, rest :: binary >>, acc),
    do: urlsafe_decode64(rest, << acc :: binary, ?/ >>)
  defp urlsafe_decode64(<< c, rest :: binary >>, acc),
    do: urlsafe_decode64(rest, << acc :: binary, c >>)
  defp urlsafe_decode64(<<>>, acc),
    do: acc

end
