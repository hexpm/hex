defmodule Hex.API do
  def get_user(username) do
    request(:get, api_url("users/#{username}"), [])
  end

  def new_user(username, email, password) do
    request(:post, api_url("users"), [],
            %{username: username, email: email, password: password})
  end

  def update_user(email, password, auth) do
    body = %{}
    if email, do: body = Map.merge(body, %{email: email})
    if password, do: body = Map.merge(body, %{password: password})

    headers = Dict.merge(auth(auth), %{'x-http-method-override' => 'PATCH'})

    request(:post, api_url("users/#{auth[:user]}"), headers, body)
  end

  def get_package(name) do
    request(:get, api_url("packages/#{name}"), [])
  end

  def new_package(name, meta, auth) do
    request(:put, api_url("packages/#{name}"), auth(auth), %{meta: meta})
  end

  def get_release(name, version) do
    request(:get, api_url("packages/#{name}/releases/#{version}"), [])
  end

  def new_release(name, tar, auth) do
    request(:post, api_url("packages/#{name}/releases"), auth(auth), tar, 'application/octet-stream')
  end

  def delete_release(name, version, auth) do
    request(:delete, api_url("packages/#{name}/releases/#{version}"), auth(auth))
  end

  def get_registry(opts \\ []) do
    if etag = opts[:etag] do
      headers = %{'if-none-match' => etag}
    end
    request(:get, cdn_url("registry.ets.gz"), headers || [])
  end

  def new_key(name, auth) do
    request(:post, api_url("keys"), auth(auth), %{name: name})
  end

  def get_keys(auth) do
    request(:get, api_url("keys"), auth(auth))
  end

  def delete_key(name, auth) do
    request(:delete, api_url("keys/#{name}"), auth(auth))
  end

  def add_package_owner(package, owner, auth) do
    owner = URI.encode_www_form(owner)
    request(:put, api_url("packages/#{package}/owners/#{owner}"), auth(auth))
  end

  def delete_package_owner(package, owner, auth) do
    owner = URI.encode_www_form(owner)
    request(:delete, api_url("packages/#{package}/owners/#{owner}"), auth(auth))
  end

  def get_package_owners(package, auth) do
    request(:get, api_url("packages/#{package}/owners"), auth(auth))
  end

  defp request(method, url, headers, body \\ nil, content_type \\ 'application/vnd.hex+elixir')
      when body == nil or is_map(body) or is_binary(body) do
    default_headers = %{
      'accept' => 'application/vnd.hex.beta+elixir',
      'accept-encoding' => 'gzip',
      'user-agent' => user_agent}
    headers = Dict.merge(default_headers, headers)
    http_opts = [timeout: 5000]
    opts = [body_format: :binary]

    cond do
      body ->
        if content_type == 'application/vnd.hex+elixir', do: body = Hex.Util.safe_serialize_elixir(body)
        request = {url, Map.to_list(headers), content_type, body}
      method in [:put, :post] ->
        request = {url, Map.to_list(headers), 'application/vnd.hex+elixir', '%{}'}
      true ->
        request = {url, Map.to_list(headers)}
    end

    case :httpc.request(method, request, http_opts, opts, :hex) do
      {:ok, response} ->
        handle_response(response)
      {:error, reason} ->
        {:http_error, reason}
    end
  end

  defp handle_response({{_version, code, _reason}, headers, body}) do
    headers = Enum.into(headers, %{})
    content_encoding = :binary.list_to_bin(headers['content-encoding'] || '')
    content_type = :binary.list_to_bin(headers['content-type'] || '')
    handle_hex_message(headers['x-hex-message'])

    if String.contains?(content_encoding, "gzip") do
      body = :zlib.gunzip(body)
    end

    if String.contains?(content_type, "application/vnd.hex+elixir") do
      body = Hex.Util.safe_deserialize_elixir(body)
    end

    {code, body}
  end

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

  def user_agent do
    'Hex/#{Hex.version} (Elixir/#{System.version})'
  end

  def cdn_url(path) do
    :binary.bin_to_list(Hex.cdn <> "/" <> path)
  end

  def url(path) do
    :binary.bin_to_list(Hex.url <> "/" <> path)
  end

  def api_url(path) do
    :binary.bin_to_list(Hex.url <> "/api/" <> path)
  end

  defp auth(key: secret) do
    %{'authorization' => String.to_char_list(secret)}
  end

  defp auth(info) do
    base64 = :base64.encode_to_string(info[:user] <> ":" <> info[:pass])
    %{'authorization' => 'Basic ' ++ base64}
  end

  @space [?\s, ?\t]

  defp parse_hex_message(message) do
    {message, rest} = skip_ws(message) |> quoted
    level = skip_ws(rest) |> opt_level
    {message, level}
  end

  defp skip_ws(<< char, rest :: binary >>) when char in @space,
    do: skip_ws(rest)
  defp skip_ws(rest),
    do: rest

  defp skip_trail_ws(input, str \\ "", ws \\ "")

  defp skip_trail_ws(<< char, rest :: binary >>, str, ws) when char in @space,
    do: skip_trail_ws(rest, str, << ws :: binary, char >>)
  defp skip_trail_ws(<< char, rest :: binary >>, str, ws),
    do: skip_trail_ws(rest, << str :: binary, ws :: binary, char >>, "")
  defp skip_trail_ws("", str, _ws),
    do: str

  defp quoted("\"" <> rest),
    do: do_quoted(rest, "")

  defp do_quoted("\"" <> rest, acc),
    do: {acc, rest}
  defp do_quoted(<< char, rest :: binary >>, acc),
    do: do_quoted(rest, << acc :: binary, char >>)

  defp opt_level(";" <> rest),
    do: do_level(rest)
  defp opt_level(_),
    do: nil

  defp do_level(rest) do
    "level" <> rest = skip_ws(rest)
    "=" <> rest = skip_ws(rest)
    skip_ws(rest) |> skip_trail_ws
  end
end
