defmodule Hex.Util do
  def ensure_registry(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path()) do
      {:error, :update_failed}
    else
      start_result = Hex.Registry.start

      # Show available newer versions
      if update_result in [{:ok, :new}, {:ok, :no_fetch}] and start_result == :ok do
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
    if update_result in [{:ok, :new}, {:ok, :no_fetch}] do
      Hex.Registry.info_installs
    end
  end

  defp update_registry(opts) do
    Hex.Util.move_home

    if Application.get_env(:hex, :registry_updated) do
      {:ok, :cached}
    else
      stopped? = Hex.Registry.stop
      Application.put_env(:hex, :registry_updated, true)

      if Keyword.get(opts, :fetch, true) do
        Hex.start_api

        path    = Hex.Registry.path
        path_gz = Hex.Registry.path <> ".gz"

        if Keyword.get(opts, :cache, true) do
          api_opts = [etag: etag(path_gz)]
        else
          api_opts = []
        end

        result =
          case Hex.API.Registry.get(api_opts) do
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
        result = {:ok, :no_fetch}
      end

      # Start registry if it was already started when update began
      if stopped? do
        Hex.Registry.start!
      end

      result
    end
  end

  def move_home do
    unless Application.get_env(:hex, :moved_home) do
      mix_home = Mix.Utils.mix_home
      hex_home = Hex.home

      File.mkdir_p!(hex_home)
      :file.rename(Path.join(mix_home, "hex.config"), Path.join(hex_home, "hex.config"))
      :file.rename(Path.join(mix_home, "hex.ets"), Path.join(hex_home, "registry.ets"))
      :file.rename(Path.join(mix_home, "hex.ets.gz"), Path.join(hex_home, "registry.ets.gz"))
      :file.rename(Path.join(mix_home, ".package-cache"), Path.join(hex_home, "packages"))

      Application.put_env(:hex, :moved_home, true)
    end

    :ok
  end

  def read_config do
    Hex.Util.move_home

    case File.read(config_path) do
      {:ok, binary} ->
        {config, _binding} = Code.eval_string(binary)
        config
      {:error, _} ->
        []
    end
  end

  def update_config(config) do
    Hex.Util.move_home

    path = config_path
    updated_config =
      case File.read(path) do
        {:ok, binary} ->
          quoted = Code.string_to_quoted!(binary, file: path)
          Keyword.merge(quoted, config)
        {:error, _} ->
          config
      end

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Macro.to_string(updated_config) <> "\n")
  end

  def write_config(config) do
    Hex.Util.move_home
    path = config_path

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Macro.to_string(config) <> "\n")
  end

  defp config_path do
    Path.join(Hex.home, "hex.config")
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
  defp binarify(atom) when is_nil(atom) or is_boolean(atom),
    do: atom
  defp binarify(atom) when is_atom(atom),
    do: Atom.to_string(atom)
  defp binarify(list) when is_list(list),
    do: for(elem <- list, do: binarify(elem))
  defp binarify(map) when is_map(map),
    do: for(elem <- map, into: %{}, do: binarify(elem))
  defp binarify(tuple) when is_tuple(tuple),
    do: for(elem <- Tuple.to_list(tuple), do: binarify(elem)) |> List.to_tuple

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
