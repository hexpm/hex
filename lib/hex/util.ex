defmodule Hex.Util do
  def ensure_registry(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path()) do
      {:error, :update_failed}
    else
      start_result = Hex.Registry.start

      # Show available newer versions
      if update_result == {:ok, :new} do
        Hex.Registry.info_installs
      end

      start_result
    end
  end

  def ensure_registry!(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path()) do
      Mix.raise "Failed to fetch registry"
    end

    Hex.Registry.start!

    # Show available newer versions
    if update_result == {:ok, :new} do
      Hex.Registry.info_installs
    end
  end

  defp update_registry(opts) do
    if Application.get_env(:hex, :registry_updated) do
      {:ok, :cached}
    else
      stopped? = Hex.Registry.stop
      Application.put_env(:hex, :registry_updated, true)

      if Keyword.get(opts, :fetch, true) do
        Hex.start_api

        path    = Hex.Registry.path
        path_gz = Hex.Registry.path <> ".gz"

        if opts[:no_cache] do
          api_opts = []
        else
          api_opts = [etag: etag(path_gz)]
        end

        result =
          case Hex.API.get_registry(api_opts) do
            {200, body} ->
              File.write!(path_gz, body)
              data = :zlib.gunzip(body)
              File.write!(path, data)
              {:ok, :new}
            {304, _} ->
              {:ok, :new}
            {code, body} ->
              Mix.shell.error("Registry update failed (#{code})")
              print_error_result(code, body)
              :error
          end
      else
        result = {:ok, :new}
      end

      # Start registry if it was already started when update began
      if stopped? do
        Hex.Registry.start!
      end

      result
    end
  end

  def etag(path) do
    case File.read(path) do
      {:ok, binary} ->
        :crypto.hash(:md5, binary)
        |> Base.encode16(case: :lower)
        |> String.to_char_list
      {:error, _} ->
        nil
    end
  end

  def safe_deserialize_elixir("") do
    nil
  end

  def safe_deserialize_elixir(string) do
    case Code.string_to_quoted(string, existing_atoms_only: true) do
      {:ok, ast} ->
        safe_eval(ast)
      _ ->
        Mix.raise "Received malformed elixir from Hex API"
    end
  end

  def safe_eval(ast) do
    if safe_term?(ast) do
      Code.eval_quoted(ast)
      |> elem(0)
      |> list_to_map
    else
      Mix.raise "Received unsafe elixir from Hex API"
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
  def safe_term?(term) when is_tuple(term), do: Enum.all?(Tuple.to_list(term), &safe_term?/1)
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
    do: Atom.to_string(atom)
  defp binarify(list) when is_list(list),
    do: for(elem <- list, do: binarify(elem))
  defp binarify(map) when is_map(map),
    do: for(elem <- map, into: %{}, do: binarify(elem))
  defp binarify(tuple) when is_tuple(tuple),
    do: for(elem <- Tuple.to_list(tuple), do: binarify(elem)) |> List.to_tuple

  defp list_to_map(list) when is_list(list) do
    if list == [] or is_tuple(List.first(list)) do
      Enum.into(list, %{}, fn
        {key, list} when is_list(list) -> {key, list_to_map(list)}
        other -> list_to_map(other)
      end)
    else
      Enum.map(list, &list_to_map/1)
    end
  end

  defp list_to_map(other) do
    other
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
      {key, map} when is_map(map) ->
        Mix.shell.info(indent(depth) <> key <> ":")
        pretty_errors(map, depth + 1)
      {key, value} ->
        Mix.shell.info(indent(depth) <> key <> ": " <> value)
    end)
  end

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)
end
