defmodule Hex.Util do
  def ensure_registry do
    stopped? = Hex.Registry.stop

    if update_registry == :error and not File.exists?(Hex.Registry.path()) do
      raise Mix.Error, message: "Failed to fetch registry"
    end
    Hex.Registry.start

    unless stopped? do
      Hex.Registry.info_installs
    end
  end

  def update_registry(opts \\ []) do
    if registry_updated? do
      { :ok, :cached }
    else
      :application.set_env(:hex, :registry_updated, true)
      Hex.start_api

      path = Hex.Registry.path
      path_gz = Hex.Registry.path <> ".gz"

      if opts[:no_cache] do
        api_opts = []
      else
        api_opts = [etag: etag(path_gz)]
      end

      case Hex.API.get_registry(api_opts) do
        { 200, body } ->
          File.write!(path_gz, body)
          data = :zlib.gunzip(body)
          File.write!(path, data)
          { :ok, :new }
        { 304, _ } ->
          { :ok, :new }
        { code, body } ->
          Mix.shell.error("Registry update failed (#{code})")
          print_error_result(code, body)
          :error
      end
    end
  end

  def registry_updated? do
    :application.get_env(:hex, :registry_updated) == { :ok, true }
  end

  def etag(path) do
    case File.read(path) do
      { :ok, binary } ->
        :crypto.hash(:md5, binary)
        |> Hex.Util.hexify
        |> List.from_char_data!
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
        safe_eval(ast)
      _ ->
        raise Mix.Error, message: "Received malformed elixir from Hex API"
    end
  end

  def safe_eval(ast) do
    if safe_term?(ast) do
      Code.eval_quoted(ast)
      |> elem(0)
      |> list_to_map
    else
      raise Mix.Error, message: "Received unsafe elixir from Hex API"
    end
  end

  def safe_term?({func, _, terms}) when func in [:{}, :%{}] and is_list(terms) do
    Enum.all?(terms, &safe_term?/1)
  end

  def safe_term?(nil), do: true
  def safe_term?(term) when is_number(term), do: true
  def safe_term?(term) when is_binary(term), do: true
  def safe_term?(term) when is_boolean(term), do: true
  def safe_term?(term) when is_list(term), do: Enum.all?(term, &safe_term?/1)
  def safe_term?(term) when is_tuple(term), do: Enum.all?(tuple_to_list(term), &safe_term?/1)
  def safe_term?(_), do: false

  def safe_serialize_elixir(term) do
    binarify(term)
    |> inspect(limit: :infinity, records: false, binaries: :as_strings)
  end

  defp binarify(binary) when is_binary(binary),
    do: binary
  defp binarify(number) when is_number(number),
    do: number
  defp binarify(atom) when nil?(atom) or is_boolean(atom),
    do: atom
  defp binarify(atom) when is_atom(atom),
    do: atom_to_binary(atom)
  defp binarify(list) when is_list(list),
    do: for(elem <- list, do: binarify(elem))
  defp binarify(map) when is_map(map),
    do: for(elem <- map, into: %{}, do: binarify(elem))
  defp binarify(tuple) when is_tuple(tuple),
    do: for(elem <- tuple_to_list(tuple), do: binarify(elem)) |> list_to_tuple

  defp list_to_map(list) when is_list(list) do
    if list == [] or is_tuple(List.first(list)) do
      Enum.into(list, %{}, fn
        { key, list } when is_list(list) -> { key, list_to_map(list) }
        other -> list_to_map(other)
      end)
    else
      Enum.map(list, &list_to_map/1)
    end
  end

  defp list_to_map(other) do
    other
  end

  def hexify(bin) do
    for << high :: size(4), low :: size(4) <- bin >>, into: "" do
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
      pretty_errors(list_to_map(body["errors"]))
    else
      Mix.shell.info(inspect(body))
    end
  end

  defp pretty_errors(errors, depth \\ 0) do
    Enum.each(errors, fn
      { key, map } when is_map(map) ->
        Mix.shell.info(indent(depth) <> key <> ":")
        pretty_errors(map, depth + 1)
      { key, value } ->
        Mix.shell.info(indent(depth) <> key <> ": " <> value)
    end)
  end

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)
end
