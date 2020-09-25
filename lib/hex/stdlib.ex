defmodule Hex.Stdlib do
  @moduledoc false

  if Version.compare(System.version(), "1.3.0") == :lt do
    def string_trim(string), do: String.strip(string)

    def string_trim_leading(string, trim) do
      trim_size = byte_size(trim)

      case string do
        <<^trim::binary-size(trim_size), rest::binary>> -> string_trim_leading(rest, trim)
        _other -> string
      end
    end

    def to_charlist(term), do: Kernel.to_char_list(term)
    def string_to_charlist(string), do: String.to_char_list(string)

    def string_pad_trailing(string, count) do
      filler_size = max(count - byte_size(string), 0)
      string <> String.duplicate(" ", filler_size)
    end
  else
    def string_trim(string), do: String.trim(string)
    def string_trim_leading(string, trim), do: String.trim_leading(string, trim)
    def string_pad_trailing(string, count), do: String.pad_trailing(string, count)
    def to_charlist(term), do: Kernel.to_charlist(term)
    def string_to_charlist(string), do: String.to_charlist(string)
  end

  if Version.compare(System.version(), "1.4.0") == :lt do
    def enum_split_with(enum, fun), do: Enum.partition(enum, fun)
  else
    def enum_split_with(enum, fun), do: Enum.split_with(enum, fun)
  end

  defmacro stacktrace() do
    if Version.compare(System.version(), "1.7.0") == :lt do
      quote do: System.stacktrace()
    else
      quote do: __STACKTRACE__
    end
  end

  # The padding: false on Base.encode64 requires Elixir v1.3+,
  # so we do a String.replace/3 instead
  def base_encode64_nopadding(binary) do
    binary
    |> Base.encode64()
    |> String.replace("=", "")
  end

  require Bitwise
  require Record

  Record.defrecordp(
    :rsa_public_key,
    :RSAPublicKey,
    Record.extract(:RSAPublicKey, from_lib: "public_key/include/public_key.hrl")
  )

  # :ssh2_pubkey requires OTP 18+
  def ssh2_pubkey_encode(rsa_public_key(modulus: n, publicExponent: e)) do
    <<string("ssh-rsa")::binary, mpint(e)::binary, mpint(n)::binary>>
  end

  defp mpint(x) when x < 0 do
    bin = int_to_bin_neg(x, [])
    string(bin)
  end

  defp mpint(x) do
    bin = int_to_bin_pos(x, [])
    <<msb, _::binary>> = bin

    if Bitwise.band(msb, 0x80) == 0x80 do
      b = <<0, bin::binary>>
      string(b)
    else
      string(bin)
    end
  end

  defp string(bin) do
    <<byte_size(bin)::32-unsigned-big, bin::binary>>
  end

  defp int_to_bin_pos(0, ds = [_ | _]), do: :erlang.list_to_binary(ds)
  defp int_to_bin_pos(x, ds), do: int_to_bin_pos(Bitwise.bsr(x, 8), [Bitwise.band(x, 255) | ds])

  defp int_to_bin_neg(-1, ds = [msb | _]) when msb >= 0x80, do: :erlang.list_to_binary(ds)
  defp int_to_bin_neg(x, ds), do: int_to_bin_neg(Bitwise.bsr(x, 8), [Bitwise.band(x, 255) | ds])

  def file_lstat(path, opts \\ []) do
    opts = Keyword.put_new(opts, :time, :universal)

    case :file.read_link_info(IO.chardata_to_string(path), opts) do
      {:ok, fileinfo} ->
        {:ok, File.Stat.from_record(fileinfo)}

      error ->
        error
    end
  end

  def file_lstat!(path, opts \\ []) do
    case file_lstat(path, opts) do
      {:ok, info} ->
        info

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "read file stats",
          path: IO.chardata_to_string(path)
    end
  end
end
