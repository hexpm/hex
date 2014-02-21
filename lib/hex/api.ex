defmodule Hex.API do
  def get_user(username) do
    request(:get, "users/#{username}", [])
  end

  def new_user(username, email, password) do
    request(:post, "users", [], [username: username, email: email, password: password])
  end

  def get_package(name) do
    request(:get, "packages/#{name}", [])
  end

  def get_packages(search) do
    query = URI.encode_query([search: search])
    request(:get, "packages?#{query}", [])
  end

  def new_package(name, meta, auth) do
    request(:put, "packages/#{name}", auth(auth), [meta: meta])
  end

  def get_release(name, version) do
    request(:get, "packages/#{name}/releases/#{version}", [])
  end

  def new_release(name, version, url, ref, reqs, auth) do
    body = [ version: version,
             git_url: url,
             git_ref: ref,
             requirements: reqs ]
    request(:post, "packages/#{name}/releases", auth(auth), body)
  end

  def get_registry(filename) do
    request_file(:get, "registry", filename)
  end

  defp request(method, path, headers, body \\ nil) do
    url = url(path)
    headers = [ { 'accept', 'application/vnd.explex.beta+elixir' } ] ++ headers
    http_opts = [timeout: 5000]
    opts = [body_format: :binary]

    if body do
      request = { url, headers, 'application/vnd.explex+elixir', safe_serialize_elixir(body) }
    else
      request = { url, headers }
    end

    case :httpc.request(method, request, http_opts, opts) do
      { :ok, response } ->
        handle_response(response)
      { :error, reason } ->
        raise Hex.Error, message: "HTTP failure: #{inspect(reason)}"
    end
  end

  defp request_file(method, path, filename) do
    url = url(path)
    headers = [ { 'accept', 'application/vnd.explex.beta+dets' } ]
    http_opts = [timeout: 5000]
    request = { url, headers }
    opts = [stream: String.to_char_list!(filename)]

    case :httpc.request(method, request, http_opts, opts) do
      { :ok, :saved_to_file } ->
        :ok
      { :error, reason } ->
        raise Hex.Error, message: "HTTP failure: #{inspect(reason)}"
    end
  end
  defp handle_response({ { _version, code, _reason }, _headers, body }) do
    { code, safe_deserialize_elixir(body) }
  end

  defp url(path) do
    String.to_char_list!(Hex.url <> "/api/" <> path)
  end

  defp auth(info) do
    base64 = :base64.encode_to_string(info[:user] <> ":" <> info[:password])
    [{ 'authorization', 'Basic ' ++ base64 }]
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
end
