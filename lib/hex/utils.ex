defmodule Hex.Utils do
  def safe_deserialize_erlang("") do
    nil
  end

  def safe_deserialize_erlang(binary) do
    case safe_binary_to_term(binary) do
      {:ok, term} ->
        term
      :error ->
        Mix.raise "Received malformed erlang from Hex API"
    end
  rescue
    ArgumentError ->
      Mix.raise "Received malformed erlang from Hex API"
  end

  def safe_serialize_erlang(term) do
    binarify(term)
    |> :erlang.term_to_binary
  end

  def safe_binary_to_term!(binary, opts \\ []) do
    case safe_binary_to_term(binary, opts) do
      {:ok, term} ->
        term
      :error ->
        raise ArgumentError, "unsafe terms"
    end
  end

  def safe_binary_to_term(binary, opts \\ [])

  def safe_binary_to_term(binary, opts) when is_binary(binary) do
    term = :erlang.binary_to_term(binary, opts)
    safe_terms(term)
    {:ok, term}
  catch
    :throw, :safe_terms ->
      :error
  end

  defp safe_terms(list) when is_list(list) do
    safe_list(list)
  end
  defp safe_terms(tuple) when is_tuple(tuple) do
    safe_tuple(tuple, tuple_size(tuple))
  end
  defp safe_terms(map) when is_map(map) do
    :maps.fold(fn key, value, acc ->
      safe_terms(key)
      safe_terms(value)
      acc
    end, map, map)
  end
  defp safe_terms(other) when is_atom(other) or is_number(other) or is_bitstring(other) or
                              is_pid(other) or is_reference(other) do
    other
  end
  defp safe_terms(_other) do
    throw :safe_terms
  end

  defp safe_list([]), do: :ok
  defp safe_list([h | t]) when is_list(t) do
    safe_terms(h)
    safe_list(t)
  end
  defp safe_list([h | t]) do
    safe_terms(h)
    safe_terms(t)
  end

  defp safe_tuple(_tuple, 0), do: :ok
  defp safe_tuple(tuple, n) do
    safe_terms(:erlang.element(n, tuple))
    safe_tuple(tuple, n - 1)
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

  def print_error_result({:error, reason}),
    do: Hex.Shell.info inspect(reason)
  def print_error_result({:ok, {status, nil, _headers}}),
    do: print_http_code(status)
  def print_error_result({:ok, {status, "", _headers}}),
    do: print_http_code(status)

  def print_error_result({:ok, {_status, body, _headers}}) when is_binary(body) do
    Hex.Shell.info body
  end

  def print_error_result({:ok, {status, body, _headers}}) when is_map(body) do
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

  def hex_package_url(package),
    do: "https://hex.pm/packages/#{package}"
  def hex_package_url(package, version),
    do: "https://hex.pm/packages/#{package}/#{version}"

  def hexdocs_url(package),
    do: "https://hexdocs.pm/#{package}"
  def hexdocs_url(package, version),
    do: "https://hexdocs.pm/#{package}/#{version}"

  def hexdocs_module_url(package, module),
    do: "https://hexdocs.pm/#{package}/#{module}.html"
  def hexdocs_module_url(package, version, module),
    do: "https://hexdocs.pm/#{package}/#{version}/#{module}.html"

  # From https://github.com/fishcakez/dialyze/blob/6698ae582c77940ee10b4babe4adeff22f1b7779/lib/mix/tasks/dialyze.ex#L168
  def otp_version do
    major = :erlang.system_info(:otp_release) |> List.to_string
    vsn_file = Path.join([:code.root_dir(), "releases", major, "OTP_VERSION"])
    try do
      {:ok, contents} = File.read(vsn_file)
      String.split(contents, "\n", trim: true)
    else
      [full] ->
        full
      _ ->
        major
    catch
      :error, _ ->
        major
    end
  end

  def lock(tuple) when elem(tuple, 0) == :hex do
    destructure [:hex, name, version, checksum, managers, deps, repo],
                Tuple.to_list(tuple)
    %{name: to_string(name),
      version: version,
      checksum: checksum,
      managers: managers,
      deps: lock_deps(deps),
      repo: repo || "hexpm"}
  end
  def lock(_), do: nil

  defp lock_deps(nil), do: nil
  defp lock_deps(deps) do
    Enum.map(deps, fn {app, req, opts} ->
      opts =
        opts
        |> Keyword.put_new(:repo, "hexpm")
        |> Keyword.update!(:hex, &to_string/1)
      {app, req, opts}
    end)
  end
end
