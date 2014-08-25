defmodule Hex.API.Util do
  @doc false
  def handle_hex_message(nil), do: :ok

  def handle_hex_message(header) do
    {message, level} = :binary.list_to_bin(header) |> parse_hex_message
    case level do
      "warn"  -> Mix.shell.info("API warning: " <> message)
      "fatal" -> Mix.shell.error("API error: " <> message)
      _       -> :ok
    end
  end

    @space [?\s, ?\t]

  def parse_hex_message(message) do
    {message, rest} = skip_ws(message) |> quoted
    level = skip_ws(rest) |> opt_level
    {message, level}
  end

  def skip_ws(<< char, rest :: binary >>) when char in @space,
    do: skip_ws(rest)
  def skip_ws(rest),
    do: rest

  def skip_trail_ws(input, str \\ "", ws \\ "")

  def skip_trail_ws(<< char, rest :: binary >>, str, ws) when char in @space,
    do: skip_trail_ws(rest, str, << ws :: binary, char >>)
  def skip_trail_ws(<< char, rest :: binary >>, str, ws),
    do: skip_trail_ws(rest, << str :: binary, ws :: binary, char >>, "")
  def skip_trail_ws("", str, _ws),
    do: str

  def quoted("\"" <> rest),
    do: do_quoted(rest, "")

  def do_quoted("\"" <> rest, acc),
    do: {acc, rest}
  def do_quoted(<< char, rest :: binary >>, acc),
    do: do_quoted(rest, << acc :: binary, char >>)

  def opt_level(";" <> rest),
    do: do_level(rest)
  def opt_level(_),
    do: nil

  def do_level(rest) do
    "level" <> rest = skip_ws(rest)
    "=" <> rest = skip_ws(rest)
    skip_ws(rest) |> skip_trail_ws
  end
end
