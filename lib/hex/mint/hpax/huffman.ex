# Vendored from hpax v1.1.0, do not edit manually

defmodule Hex.Mint.HPAX.Huffman do
  @moduledoc false

  import Bitwise, only: [>>>: 2]

  # This file is downloaded from the spec directly.
  # http://httpwg.org/specs/rfc7541.html#huffman.code
  table_file = Path.absname("huffman_table", __DIR__)
  @external_resource table_file

  entries =
    Enum.map(File.stream!(table_file), fn line ->
      [byte_value, bits, _hex, bit_count] =
        line
        |> case do
          <<?', _, ?', ?\s, rest::binary>> -> rest
          "EOS " <> rest -> rest
          _other -> line
        end
        |> String.replace(["|", "(", ")", "[", "]"], "")
        |> String.split()

      byte_value = String.to_integer(byte_value)
      bits = String.to_integer(bits, 2)
      bit_count = String.to_integer(bit_count)

      {byte_value, bits, bit_count}
    end)

  {regular_entries, [eos_entry]} = Enum.split(entries, -1)
  {_eos_byte_value, eos_bits, eos_bit_count} = eos_entry

  ## Encoding

  @spec encode(binary()) :: binary()
  def encode(binary) do
    encode(binary, _acc = <<>>)
  end

  for {byte_value, bits, bit_count} <- regular_entries do
    defp encode(<<unquote(byte_value), rest::binary>>, acc) do
      encode(rest, <<acc::bitstring, unquote(bits)::size(unquote(bit_count))>>)
    end
  end

  defp encode(<<>>, acc) do
    overflowing_bits = rem(bit_size(acc), 8)

    if overflowing_bits == 0 do
      acc
    else
      bits_to_add = 8 - overflowing_bits

      value_of_bits_to_add =
        take_significant_bits(unquote(eos_bits), unquote(eos_bit_count), bits_to_add)

      <<acc::bitstring, value_of_bits_to_add::size(bits_to_add)>>
    end
  end

  ## Decoding

  @spec decode(binary()) :: binary()
  def decode(binary) when is_bitstring(binary) do
    decode(binary, <<>>)
  end

  for {byte_value, bits, bit_count} <- regular_entries do
    defp decode(<<unquote(bits)::size(unquote(bit_count)), rest::bitstring>>, acc) do
      decode(rest, <<acc::binary, unquote(byte_value)>>)
    end
  end

  defp decode(<<>>, acc) do
    acc
  end

  # Use binary syntax for single match context optimization.
  defp decode(<<padding::bitstring>>, acc) when bit_size(padding) in 1..7 do
    padding_size = bit_size(padding)
    <<padding::size(padding_size)>> = padding

    if take_significant_bits(unquote(eos_bits), unquote(eos_bit_count), padding_size) == padding do
      acc
    else
      throw({:hpax, {:protocol_error, :invalid_huffman_encoding}})
    end
  end

  # Anything left over here is 8 or more bits that don't match any Huffman code (the clauses
  # above only match a complete code, the empty binary, or up to 7 bits of valid EOS padding).
  # This can only happen with a malformed/malicious encoding, since a real encoder never
  # produces output with more than 7 trailing bits that aren't a complete code.
  defp decode(<<_rest::bitstring>>, _acc) do
    throw({:hpax, {:protocol_error, :invalid_huffman_encoding}})
  end

  ## Helpers

  @compile {:inline, take_significant_bits: 3}
  defp take_significant_bits(value, bit_count, bits_to_take) do
    value >>> (bit_count - bits_to_take)
  end
end
