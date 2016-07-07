defmodule Hex.Crypto.PBES2_HMAC_SHA2_AES_KW do
  @moduledoc ~S"""
  Key Encryption with PBES2.

  See: https://tools.ietf.org/html/rfc7518#section-4.8
  See: https://tools.ietf.org/html/rfc2898#section-6.2
  """

  alias Hex.Crypto.AES_KW
  alias Hex.Crypto.PKCS5

  @spec key_encrypt(binary, String.t, binary, pos_integer, 16 | 24 | 32) :: binary
  def key_encrypt(decrypted_key, password, salt_input, iterations, derived_key_length)
      when is_binary(decrypted_key)
      and is_binary(password)
      and is_binary(salt_input)
      and is_integer(iterations) and iterations >= 1
      and derived_key_length in [16, 24, 32] do
    salt = wrap_salt_input(salt_input, derived_key_length)
    hash = case derived_key_length do
      16 -> :sha256
      24 -> :sha384
      32 -> :sha512
    end
    derived_key = PKCS5.pbkdf2(password, salt, iterations, derived_key_length, hash)
    AES_KW.wrap(decrypted_key, derived_key)
  end

  @spec key_decrypt(binary, String.t, binary, pos_integer, 16 | 24 | 32) :: {:ok, binary} | :error
  def key_decrypt(encrypted_key, password, salt_input, iterations, derived_key_length)
      when is_binary(encrypted_key)
      and is_binary(password)
      and is_binary(salt_input)
      and is_integer(iterations) and iterations >= 1
      and derived_key_length in [16, 24, 32] do
    salt = wrap_salt_input(salt_input, derived_key_length)
    hash = case derived_key_length do
      16 -> :sha256
      24 -> :sha384
      32 -> :sha512
    end
    derived_key = PKCS5.pbkdf2(password, salt, iterations, derived_key_length, hash)
    AES_KW.unwrap(encrypted_key, derived_key)
  end

  ## Key Manager

  @behaviour Hex.Crypto.KeyManager

  def init(options) do
    case Keyword.fetch(options, :password) do
      {:ok, password} when is_binary(password) and byte_size(password) > 0 ->
        case Keyword.fetch(options, :derived_key_length) do
          {:ok, derived_key_length} when derived_key_length in [16, 24, 32] ->
            case Keyword.fetch(options, :salt) do
              {:ok, salt} when is_binary(salt) ->
                salt
              _ ->
                case Keyword.get(options, :salt_length, 16) do
                  salt_length when is_integer(salt_length) and salt_length >= 0 ->
                    :crypto.strong_rand_bytes(salt_length)
                  _ ->
                    {:error, ":salt_length must be a non negative integer"}
                end
            end
            |> case do
              salt when is_binary(salt) ->
                case Keyword.get(options, :iterations, 1000) do
                  iterations when is_integer(iterations) and iterations >= 1 ->
                    params = %{
                      password: password,
                      salt: salt,
                      iterations: iterations,
                      derived_key_length: derived_key_length
                    }
                    {:ok, params}
                  _ ->
                    {:error, ":iterations must be a positive integer"}
                end
              salt_error ->
                salt_error
            end
          _ ->
            {:error, ":derived_key_length is required and must be 16, 24, or 32"}
        end
      _ ->
        {:error, ":password is required and must be a non-empty binary"}
    end
  end

  def encrypt(decrypted_key, %{password: password, salt: salt, iterations: iterations, derived_key_length: derived_key_length}) do
    key_encrypt(decrypted_key, password, salt, iterations, derived_key_length)
  end

  def decrypt(encrypted_key, %{password: password, salt: salt, iterations: iterations, derived_key_length: derived_key_length}) do
    key_decrypt(encrypted_key, password, salt, iterations, derived_key_length)
  end

  def encode(%{salt: salt, iterations: iterations, derived_key_length: derived_key_length}) do
    algorithm =
      case derived_key_length do
        16 -> "PBES2-HS256+A128KW"
        24 -> "PBES2-HS384+A192KW"
        32 -> "PBES2-HS512+A256KW"
      end
    params = salt
    |> Base.url_encode64(padding: false)
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(:binary.encode_unsigned(iterations, :big), padding: false))
    {algorithm, params}
  end

  def decode(algorithm, params, options) when algorithm in ["PBES2-HS256+A128KW", "PBES2-HS384+A192KW", "PBES2-HS512+A256KW"] do
    derived_key_length =
      case algorithm do
        "PBES2-HS256+A128KW" -> 16
        "PBES2-HS384+A192KW" -> 24
        "PBES2-HS512+A256KW" -> 32
      end
    case String.split(params, ".", parts: 2) do
      [salt, iterations] ->
        case Base.url_decode64(salt, padding: false) do
          {:ok, salt} ->
            case Base.url_decode64(iterations, padding: false) do
              {:ok, iterations} ->
                try do
                  iterations = :binary.decode_unsigned(iterations, :big)
                  options = options
                  |> Keyword.put(:salt, salt)
                  |> Keyword.put(:iterations, iterations)
                  |> Keyword.put(:derived_key_length, derived_key_length)
                  init(options)
                catch
                  _,_ ->
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
  def decode(_, _, _),
    do: :error

  ## Internal

  defp wrap_salt_input(salt_input, 16),
    do: << "PBES2-HS256+A128KW", 0, salt_input :: binary >>
  defp wrap_salt_input(salt_input, 24),
    do: << "PBES2-HS384+A192KW", 0, salt_input :: binary >>
  defp wrap_salt_input(salt_input, 32),
    do: << "PBES2-HS512+A256KW", 0, salt_input :: binary >>
end