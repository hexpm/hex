defmodule Hex.Utils do
  @moduledoc false

  def safe_deserialize_erlang("") do
    nil
  end

  def safe_deserialize_erlang(binary) do
    case safe_binary_to_term(binary) do
      {:ok, term} ->
        term

      :error ->
        Mix.raise("Received malformed erlang from Hex API")
    end
  rescue
    ArgumentError ->
      Mix.raise("Received malformed erlang from Hex API")
  end

  def safe_serialize_erlang(term) do
    binarify(term)
    |> :erlang.term_to_binary()
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
    fun = fn key, value, acc ->
      safe_terms(key)
      safe_terms(value)
      acc
    end

    :maps.fold(fun, map, map)
  end

  defp safe_terms(other)
       when is_atom(other) or is_number(other) or is_bitstring(other) or is_pid(other) or
              is_reference(other) do
    other
  end

  defp safe_terms(_other) do
    throw(:safe_terms)
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

  def truncate(string, options \\ []) do
    length = options[:length] || 50
    omission = options[:omission] || "..."

    cond do
      not String.valid?(string) ->
        string

      String.length(string) < length ->
        string

      true ->
        String.slice(string, 0, length) <> omission
    end
  end

  def binarify(term, opts \\ [])

  def binarify(binary, _opts) when is_binary(binary) do
    binary
  end

  def binarify(number, _opts) when is_number(number) do
    number
  end

  def binarify(atom, _opts) when is_nil(atom) or is_boolean(atom) do
    atom
  end

  def binarify(atom, _opts) when is_atom(atom) do
    Atom.to_string(atom)
  end

  def binarify(list, opts) when is_list(list) do
    for(elem <- list, do: binarify(elem, opts))
  end

  def binarify(tuple, opts) when is_tuple(tuple) do
    for(elem <- Tuple.to_list(tuple), do: binarify(elem, opts))
    |> List.to_tuple()
  end

  def binarify(map, opts) when is_map(map) do
    if Keyword.get(opts, :maps, true) do
      for(elem <- map, into: %{}, do: binarify(elem, opts))
    else
      for(elem <- map, do: binarify(elem, opts))
    end
  end

  def print_error_result({:error, reason}) do
    Hex.Shell.info(inspect(reason))
  end

  def print_error_result({:ok, {status, nil, _headers}}) do
    print_http_code(status)
  end

  def print_error_result({:ok, {status, "", _headers}}) do
    print_http_code(status)
  end

  def print_error_result({:ok, {_status, body, _headers}}) when is_binary(body) do
    Hex.Shell.info(body)
  end

  def print_error_result({:ok, {status, body, _headers}}) when is_map(body) do
    message = body["message"]
    errors = body["errors"]

    if message do
      Hex.Shell.info(message)
    end

    if errors do
      pretty_errors(errors)
    end

    unless message || errors do
      print_http_code(status)
      Hex.Shell.info(body)
    end
  end

  defp pretty_errors(errors, depth \\ 0) do
    Enum.each(errors, fn
      {key, map} when is_map(map) ->
        Hex.Shell.info(indent(depth) <> key <> ":")
        pretty_errors(map, depth + 1)

      {key, value} ->
        message = pretty_error_message(value, depth)
        Hex.Shell.info(indent(depth) <> key <> ": " <> message)
    end)
  end

  defp pretty_error_message(message, depth) do
    if message =~ "\n" do
      message =
        message
        |> String.trim()
        |> String.replace("\n", "\n" <> indent(depth + 1))

      "\n" <> indent(depth + 1) <> message
    else
      message
    end
  end

  defp print_http_code(code), do: Hex.Shell.info(pretty_http_code(code))

  defp pretty_http_code(401), do: "Authentication failed (401)"
  defp pretty_http_code(403), do: "Forbidden (403)"
  defp pretty_http_code(404), do: "Entity not found (404)"
  defp pretty_http_code(422), do: "Validation failed (422)"
  defp pretty_http_code(code), do: "HTTP status code: #{code}"

  defp indent(0), do: "  "
  defp indent(depth), do: "  " <> indent(depth - 1)

  def hexdocs_url(organization, package)
      when organization in ["hexpm", nil],
      do: "https://hexdocs.pm/#{package}"

  def hexdocs_url(organization, package),
    do: "https://#{organization}.hexdocs.pm/#{package}"

  def hexdocs_url(organization, package, version)
      when organization in ["hexpm", nil],
      do: "https://hexdocs.pm/#{package}/#{version}"

  def hexdocs_url(organization, package, version),
    do: "https://#{organization}.hexdocs.pm/#{package}/#{version}"

  def hexdocs_module_url(organization, package, module)
      when organization in ["hexpm", nil],
      do: "https://hexdocs.pm/#{package}/#{module}.html"

  def hexdocs_module_url(organization, package, module),
    do: "https://#{organization}.hexdocs.pm/#{package}/#{module}.html"

  def hexdocs_module_url(organization, package, version, module)
      when organization in ["hexpm", nil],
      do: "https://hexdocs.pm/#{package}/#{version}/#{module}.html"

  def hexdocs_module_url(organization, package, version, module),
    do: "https://#{organization}.hexdocs.pm/#{package}/#{version}/#{module}.html"

  def package_retirement_reason(:RETIRED_OTHER), do: "other"
  def package_retirement_reason(:RETIRED_INVALID), do: "invalid"
  def package_retirement_reason(:RETIRED_SECURITY), do: "security"
  def package_retirement_reason(:RETIRED_DEPRECATED), do: "deprecated"
  def package_retirement_reason(:RETIRED_RENAMED), do: "renamed"
  def package_retirement_reason(other), do: other

  def package_retirement_message(%{reason: reason_code, message: message}) do
    "(#{package_retirement_reason(reason_code)}) #{message}"
  end

  def package_retirement_message(%{reason: reason_code}) do
    "(#{package_retirement_reason(reason_code)})"
  end

  # From https://github.com/fishcakez/dialyze/blob/6698ae582c77940ee10b4babe4adeff22f1b7779/lib/mix/tasks/dialyze.ex#L168
  def otp_version do
    major = :erlang.system_info(:otp_release) |> List.to_string()
    vsn_file = Path.join([:code.root_dir(), "releases", major, "OTP_VERSION"])

    try do
      {:ok, contents} = File.read(vsn_file)
      String.split(contents, "\n", trim: true)
    else
      [full] -> full
      _ -> major
    catch
      :error, _ -> major
    end
  end

  def windows_repo_path_fix(path) do
    case :os.type() do
      {:win32, _name} -> String.replace(path, ":", "-")
      {_family, _name} -> path
    end
  end

  def lock(tuple) when elem(tuple, 0) == :hex do
    if tuple_size(tuple) > 8 and Hex.Server.should_warn_lock_version?() do
      Hex.Shell.warn(
        "The mix.lock file was generated with a newer version of Hex. Update " <>
          "your client by running `mix local.hex` to avoid losing data."
      )
    end

    destructure(
      [:hex, name, version, inner_checksum, managers, deps, repo, outer_checksum],
      Tuple.to_list(tuple)
    )

    %{
      name: to_string(name),
      version: version,
      inner_checksum: inner_checksum,
      outer_checksum: outer_checksum,
      managers: managers,
      deps: lock_deps(deps),
      repo: repo || "hexpm"
    }
  end

  def lock(_) do
    nil
  end

  defp lock_deps(nil) do
    nil
  end

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
