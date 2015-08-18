defmodule Hex.Utils do
  def ensure_registry(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path) do
      {:error, :update_failed}
    else
      start_result = Hex.Registry.open

      # Show available newer versions
      if update_result in [{:ok, :new}, {:ok, :no_fetch}] and start_result == :ok do
        Hex.Registry.info_installs
      end

      start_result
    end
  end

  def ensure_registry!(opts \\ []) do
    update_result = update_registry(opts)

    if update_result == :error and not File.exists?(Hex.Registry.path) do
      Mix.raise "Failed to fetch registry"
    end

    Hex.Registry.open!

    # Show available newer versions
    if update_result in [{:ok, :new}, {:ok, :no_fetch}] do
      Hex.Registry.info_installs
    end
  end

  defp update_registry(opts) do
    cond do
      Hex.State.fetch!(:offline?) ->
        {:ok, :offline}
      Hex.State.fetch!(:registry_updated) ->
        {:ok, :cached}
      true ->
        Hex.State.put(:registry_updated, true)

        closed? = Hex.Registry.close
        path    = Hex.Registry.path
        path_gz = path <> ".gz"
        fetch?  = Keyword.get(opts, :fetch, true) and
                  (Keyword.get(opts, :update, true) or not week_fresh?(path_gz))
        try do
          if fetch? do
            if Keyword.get(opts, :cache, true) do
              api_opts = [etag: etag(path_gz)]
            else
              api_opts = []
            end

            case Hex.API.Registry.get(api_opts) do
              {200, body} ->
                File.mkdir_p!(Path.dirname(path))
                File.write!(path_gz, body)
                data = :zlib.gunzip(body)
                File.write!(path, data)
                {:ok, :new}
              {304, _} ->
                {:ok, :new}
              {code, body} ->
                Hex.Shell.error "Registry update failed (#{code})"
                print_error_result(code, body)
                :error
            end
          else
            {:ok, :no_fetch}
          end
        after
          # Open registry if it was already open when update began
          if closed?, do: Hex.Registry.open!
        end
    end
  end

  @week_seconds 7 * 24 * 60 * 60

  def week_fresh?(path) do
    case File.stat(path) do
      {:ok, %File.Stat{mtime: mtime}} ->
        now   = :calendar.local_time |> :calendar.datetime_to_gregorian_seconds
        mtime = mtime                |> :calendar.datetime_to_gregorian_seconds

        now - mtime < @week_seconds
      {:error, _} ->
        false
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

  def binarify(term, opts \\ [])

  def binarify(binary, _opts) when is_binary(binary),
    do: binary
  def binarify(number, _opts) when is_number(number),
    do: number
  def binarify(atom, _opts) when is_nil(atom) or is_boolean(atom),
    do: atom
  def binarify(atom, _opts) when is_atom(atom),
    do: Atom.to_string(atom)
  def binarify(list, opts) when is_list(list),
    do: for(elem <- list, do: binarify(elem, opts))
  def binarify(tuple, opts) when is_tuple(tuple),
    do: for(elem <- Tuple.to_list(tuple), do: binarify(elem, opts)) |> List.to_tuple
  def binarify(map, opts) when is_map(map) do
    if Keyword.get(opts, :maps, true) do
      for(elem <- map, into: %{}, do: binarify(elem, opts))
    else
      for(elem <- map, do: binarify(elem, opts))
    end
  end

  def print_error_result(:http_error, reason),
    do: Hex.Shell.info inspect(reason)
  def print_error_result(status, nil),
    do: print_http_code(status)
  def print_error_result(status, ""),
    do: print_http_code(status)

  def print_error_result(status, body) do
    message = body["message"]
    errors = body["errors"]

    if message do
      Hex.Shell.info message
    end

    if errors do
      pretty_errors(errors)
    end

    unless message || errors do
      print_http_code(status)
      Hex.Shell.info body
    end
  end

  defp pretty_errors(errors, depth \\ 0) do
    Enum.each(errors, fn
      {key, map} when is_map(map) ->
        Hex.Shell.info indent(depth) <> key <> ":"
        pretty_errors(map, depth + 1)
      {key, value} ->
        Hex.Shell.info indent(depth) <> key <> ": " <> value
    end)
  end

  defp print_http_code(code), do: Hex.Shell.info pretty_http_code(code)

  defp pretty_http_code(401), do: "Authentication failed (401)"
  defp pretty_http_code(403), do: "Forbidden (403)"
  defp pretty_http_code(404), do: "Entity not found (404)"
  defp pretty_http_code(422), do: "Validation failed (422)"
  defp pretty_http_code(code), do: "HTTP status code: #{code}"

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)

  def hex_package_url(package, version) do
    "https://hex.pm/packages/#{package}/#{version}"
  end

  def hexdocs_url(package, version) do
    "http://hexdocs.pm/#{package}/#{version}/"
  end
end
