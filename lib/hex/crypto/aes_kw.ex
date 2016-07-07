defmodule Hex.Crypto.AES_KW do
  @moduledoc ~S"""
  Advanced Encryption Standard (AES) Key Wrap Algorithm.

  A Key Management Mode in which the Content Encryption Key (CEK)
  value is encrypted with a symmetric key wrapping algorithm that
  takes a Key-Encryption Key (KEK) and Initial Value (IV) to wrap
  the plain-text CEK using AES-ECB mode.

  See: https://tools.ietf.org/html/rfc3394
  See: http://csrc.nist.gov/groups/ST/toolkit/documents/kms/key-wrap.pdf
  """

  use Bitwise

  @default_iv << 0xA6A6A6A6A6A6A6A6 :: 1-unsigned-big-integer-unit(64) >>

  @doc """
  Given a plain-text, key-encryption key, and initial value, returns
  the cipher-text (encrypted plain-text).

  The plain-text byte size must be a multiple of 8.

  The key-encryption key must be an AES key of 128, 192, or 256 bits.

  The initial value may be any size, but 16 bytes or larger is recommended.
  """
  @spec wrap(<<_::_ * 64>>, <<_::16>> | <<_::24>> | <<_::32>>, String.t) :: String.t
  def wrap(plain_text, kek, iv \\ @default_iv)
      when rem(byte_size(plain_text), 8) === 0
      and bit_size(kek) in [128, 192, 256]
      and is_binary(iv) do
    buffer = iv <> plain_text
    block_count = div(byte_size(buffer), 8) - 1
    wrap(buffer, 0, block_count, kek)
  end

  @doc """
  Given a cipher-text, key-encryption key, and initial value, returns
  the plain-text (decrypted cipher-text) or an error.

  The cipher-text byte size must be a multiple of 8.

  The key-encryption key must be an AES key of 128, 192, or 256 bits.

  The initial value may be any size, but 16 bytes or larger is recommended.
  """
  @spec unwrap(<<_::_ * 64>>, <<_::16>> | <<_::24>> | <<_::32>>, String.t) :: {:ok, String.t} | :error
  def unwrap(cipher_text, kek, iv \\ @default_iv)
      when rem(byte_size(cipher_text), 8) === 0
      and bit_size(kek) in [128, 192, 256]
      and is_binary(iv) do
    block_count = div(byte_size(cipher_text), 8) - 1
    iv_size = byte_size(iv)
    case unwrap(cipher_text, 5, block_count, kek) do
      << ^iv :: binary-size(iv_size), plain_text :: binary >> ->
        {:ok, plain_text}
      _ ->
        :error
    end
  end

  ## Key Manager

  @behaviour Hex.Crypto.KeyManager

  def init(options) do
    case Keyword.fetch(options, :kek) do
      {:ok, kek} when is_binary(kek) and bit_size(kek) in [128, 192, 256] ->
        iv = Keyword.get(options, :iv, @default_iv)
        if is_binary(iv) do
          params = %{
            kek: kek,
            iv: iv
          }
          {:ok, params}
        else
          {:error, ":iv must be a bitstring"}
        end
      _ ->
        {:error, ":kek is required, must be a bitstring, and must be 128, 192, or 256 bits"}
    end
  end

  def encrypt(decrypted_key, %{kek: kek, iv: iv}) do
    wrap(decrypted_key, kek, iv)
  end

  def decrypt(encrypted_key, %{kek: kek, iv: iv}) do
    unwrap(encrypted_key, kek, iv)
  end

  def encode(%{kek: kek}) do
    algorithm =
      case bit_size(kek) do
        128 -> "A128KW"
        192 -> "A192KW"
        256 -> "A256KW"
      end
    {algorithm, <<>>}
  end

  def decode(algorithm, <<>>, options) when algorithm in ["A128KW", "A192KW", "A256KW"] do
    init(options)
  end
  def decode(_, _, _),
    do: :error

  ## Internal

  # AES-ECB is only supported in OTP 18+, however,
  # AES-CBC with an all-zero IV is equivalent to AES-ECB.
  defp aes_ecb_encrypt(key, plain_text) do
    try do
      :crypto.block_encrypt(:aes_ecb, key, plain_text)
    catch
      _,_ ->
        cipher =
          case bit_size(key) do
            128 -> :aes_cbc128
            192 -> :aes_cbc192
            256 -> :aes_cbc256
          end
        iv = << 0 :: 128 >>
        :crypto.block_encrypt(cipher, key, iv, plain_text)
    end
  end

  # AES-ECB is only supported in OTP 18+, however,
  # AES-CBC with an all-zero IV is equivalent to AES-ECB.
  defp aes_ecb_decrypt(key, cipher_text) do
    try do
      :crypto.block_decrypt(:aes_ecb, key, cipher_text)
    catch
      _,_ ->
        cipher =
          case bit_size(key) do
            128 -> :aes_cbc128
            192 -> :aes_cbc192
            256 -> :aes_cbc256
          end
        iv = << 0 :: 128 >>
        :crypto.block_decrypt(cipher, key, iv, cipher_text)
    end
  end

  defp wrap(buffer, 6, _block_count, _kek),
    do: buffer
  defp wrap(buffer, j, block_count, kek) do
    buffer
    |> wrap(j, 1, block_count, kek)
    |> wrap(j + 1, block_count, kek)
  end

  defp wrap(buffer, _j, i, block_count, _kek) when i > block_count,
    do: buffer
  defp wrap(<< a :: binary-size(8), rest :: binary >>, j, i, block_count, kek) do
    head_size = (i - 1) * 8
    <<
      head :: binary-size(head_size),
      b    :: binary-size(8),
      tail :: binary
    >> = rest
    round = (block_count * j) + i
    data = a <> b
    <<
      a :: 1-unsigned-big-integer-unit(64),
      b :: binary
    >> = aes_ecb_encrypt(kek, data)
    a = a ^^^ round
    <<
      a    :: 1-unsigned-big-integer-unit(64),
      head :: binary,
      b    :: binary,
      tail :: binary
    >>
    |> wrap(j, i + 1, block_count, kek)
  end

  defp unwrap(buffer, j, _block_count, _kek) when j < 0,
    do: buffer
  defp unwrap(buffer, j, block_count, kek) do
    buffer
    |> unwrap(j, block_count, block_count, kek)
    |> unwrap(j - 1, block_count, kek)
  end

  defp unwrap(buffer, _j, i, _block_count, _kek) when i < 1,
    do: buffer
  defp unwrap(<< a :: 1-unsigned-big-integer-unit(64), rest :: binary >>, j, i, block_count, kek) do
    head_size = (i - 1) * 8
    <<
      head :: binary-size(head_size),
      b    :: binary-size(8),
      tail :: binary
    >> = rest
    round = (block_count * j) + i
    a = a ^^^ round
    data = << a :: 1-unsigned-big-integer-unit(64), b :: binary >>
    <<
      a :: binary-size(8),
      b :: binary
    >> = aes_ecb_decrypt(kek, data)
    <<
      a    :: binary,
      head :: binary,
      b    :: binary,
      tail :: binary
    >>
    |> unwrap(j, i - 1, block_count, kek)
  end
end