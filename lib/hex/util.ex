defmodule Hex.Util do
  def update_registry(info \\ nil) do
    if :application.get_env(:hex, :registry_updated) == { :ok, true } do
      { :ok, :cached }
    else
      :application.set_env(:hex, :registry_updated, true)

      if info, do: Mix.shell.info(info)

      path = Hex.Registry.path
      path_gz = Hex.Registry.path <> ".gz"

      opts = [etag: Hex.Util.etag(path_gz)]

      case Hex.API.get_registry(opts) do
        { 200, body } ->
          File.write!(path_gz, body)
          data = :zlib.gunzip(body)
          File.write!(path, data)
          Mix.shell.info("Registry update was successful!")
          { :ok, :new }
        { 304, _ } ->
          Mix.shell.info("Registry was fresh!")
          { :ok, :new }
        { code, body } ->
          Mix.shell.error("Registry update failed! (#{code})")
          Mix.Tasks.Hex.Util.print_error_result(code, body)
          :error
      end
    end
  end

  def etag(path) do
    case File.read(path) do
      { :ok, binary } ->
        :crypto.hash(:md5, binary)
        |> Hex.Util.hexify
        |> String.to_char_list!
      { :error, _ } ->
        nil
    end
  end

  def safe_deserialize_elixir("") do
    nil
  end

  def safe_deserialize_elixir(string) do
    case Code.string_to_quoted(string, existing_atoms_only: true) do
      { :ok, ast } ->
        if Macro.safe_term(ast) do
          Code.eval_quoted(ast) |> elem(0)
        else
          raise Hex.Error, message: "received unsafe elixir from API"
        end
      _ ->
        raise Hex.Error, message: "received malformed elixir from API"
    end
  end

  def safe_serialize_elixir(term) do
    binarify(term)
    |> inspect(limit: :infinity, records: false, binaries: :as_strings)
  end

  defp binarify(binary) when is_binary(binary),
    do: binary
  defp binarify(atom) when is_atom(atom),
    do: atom_to_binary(atom)
  defp binarify(list) when is_list(list),
    do: lc(elem inlist list, do: binarify(elem))
  defp binarify({ left, right }),
    do: { binarify(left), binarify(right) }

  def hexify(bin) do
    bc << high :: size(4), low :: size(4) >> inbits bin do
      << hex_char(high), hex_char(low) >>
    end
  end

  defp hex_char(n) when n < 10, do: ?0 + n
  defp hex_char(n) when n < 16, do: ?a - 10 + n

  def dehexify(bin) do
    int  = :erlang.binary_to_integer(bin, 16)
    size = byte_size(bin)
    << int :: [integer, unit(4), size(size)] >>
  end

  def print_error_result(:http_error, reason) do
    Mix.shell.info(inspect(reason))
  end

  def print_error_result(_status, nil), do: :ok
  def print_error_result(_status, ""), do: :ok

  def print_error_result(_status, body) do
    if body["message"] && body["errors"] do
      Mix.shell.info(body["message"])
      pretty_errors(body["errors"])
    else
      Mix.shell.info(inspect(body))
    end
  end

  defp pretty_errors(errors, depth \\ 0) do
    Enum.each(errors, fn
      { key, list } when is_list(list) ->
        Mix.shell.info(indent(depth) <> key <> ":")
        pretty_errors(errors, depth + 1)
      { key, value } ->
        Mix.shell.info(indent(depth) <> key <> ": " <> value)
    end)
  end

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)
end
