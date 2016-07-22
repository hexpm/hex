defmodule Hex.Crypto.PBES2_HMAC_SHA2 do
  @moduledoc ~S"""
  Direct Key Derivation with PBES2 and HMAC-SHA-2.

  See: https://tools.ietf.org/html/rfc7518#section-4.8
  See: https://tools.ietf.org/html/rfc2898#section-6.2
  """

  alias Hex.Crypto.ContentEncryptor
  alias Hex.Crypto.KeyManager
  alias Hex.Crypto.PKCS5

  @spec derive_key(String.t, binary, pos_integer, non_neg_integer, :sha256 | :sha384 | :sha512) :: binary
  def derive_key(password, salt_input, iterations, derived_key_length, hash)
      when is_binary(password)
      and is_binary(salt_input)
      and is_integer(iterations) and iterations >= 1
      and is_integer(derived_key_length) and derived_key_length >= 0
      and hash in [:sha256, :sha384, :sha512] do
    salt = wrap_salt_input(salt_input, hash)
    derived_key = PKCS5.pbkdf2(password, salt, iterations, derived_key_length, hash)
    derived_key
  end

  ## Key Manager

  @behaviour KeyManager

  def init(protected = %{ alg: alg }, options)
      when alg in ["PBES2-HS256", "PBES2-HS384", "PBES2-HS512"] do
    hash =
      case alg do
        "PBES2-HS256" -> :sha256
        "PBES2-HS384" -> :sha384
        "PBES2-HS512" -> :sha512
      end
    case Keyword.fetch(options, :password) do
      {:ok, password} when is_binary(password) ->
        case Map.fetch(protected, :p2c) do
          {:ok, iterations} when is_integer(iterations) and iterations >= 1 ->
            case Map.fetch(protected, :p2s) do
              {:ok, salt} when is_binary(salt) ->
                params = %{
                  hash: hash,
                  password: password
                }
                {:ok, params}
              _ ->
                {:error, "protected :p2s (PBKDF2 salt) must be a binary"}
            end
          _ ->
            {:error, "protected :p2c (PBKDF2 iterations) must be a positive integer"}
        end
      _ ->
        {:error, "option :password (PBKDF2 password) must be a binary"}
    end
  end

  def encrypt(%{password: password, hash: hash}, %{p2c: iterations, p2s: salt} = protected, content_encryptor) do
    derived_key_length = ContentEncryptor.key_length(content_encryptor)
    key = derive_key(password, salt, iterations, derived_key_length, hash)
    encrypted_key = <<>>
    {:ok, protected, key, encrypted_key}
  end

  def decrypt(%{password: password, hash: hash}, %{p2c: iterations, p2s: salt}, <<>>, content_encryptor) do
    derived_key_length = ContentEncryptor.key_length(content_encryptor)
    key = derive_key(password, salt, iterations, derived_key_length, hash)
    {:ok, key}
  end
  def decrypt(_, _, _, _),
    do: :error

  ## Internal

  defp wrap_salt_input(salt_input, :sha256),
    do: << "PBES2-HS256", 0, salt_input :: binary >>
  defp wrap_salt_input(salt_input, :sha384),
    do: << "PBES2-HS384", 0, salt_input :: binary >>
  defp wrap_salt_input(salt_input, :sha512),
    do: << "PBES2-HS512", 0, salt_input :: binary >>

end