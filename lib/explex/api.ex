defmodule Explex.API do
  def get_user(username) do
    request(:get, "users/#{username}")
  end

  def new_user(username, email, password) do
    request(:post, "users", [username: username, email: email, password: password])
  end

  defp request(method, path, body \\ nil) do
    url = url(path)
    headers = [ { 'accept', 'application/vnd.explex.beta+elixir' } ]
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
        raise Explex.Error, message: "HTTP failure: #{inspect(reason)}"
    end
  end

  defp handle_response({ { _version, code, _reason }, _headers, body }) do
    { code, safe_deserialize_elixir(body) }
  end

  defp url(path) do
    String.to_char_list!(Explex.url <> "/api/" <> path)
  end

  def safe_deserialize_elixir(string) do
    case Code.string_to_quoted(string, existing_atoms_only: true) do
      { :ok, ast } ->
        if Macro.safe_term(ast) do
          Code.eval_quoted(ast) |> elem(0)
        else
          raise Explex.Error, message: "received unsafe elixir from API"
        end
      _ ->
        raise Explex.Error, message: "received malformed elixir from API"
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
