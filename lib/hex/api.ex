defmodule Hex.API do
  alias Hex.API.Util

  def request(method, url, headers, body \\ nil) when body == nil or is_map(body) do
    default_headers = %{
      'accept' => 'application/vnd.hex.beta+elixir',
      'accept-encoding' => 'gzip',
      'user-agent' => user_agent}
    headers = Dict.merge(default_headers, headers)
    http_opts = [timeout: 5000]
    opts = [body_format: :binary]

    cond do
      body ->
        body = Hex.Util.safe_serialize_elixir(body)
        request = {url, Map.to_list(headers), 'application/vnd.hex+elixir', body}
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

  @chunk 10_000

  def request_tar(method, url, headers, body, progress) do
    default_headers = %{
      'accept' => 'application/vnd.hex.beta+elixir',
      'accept-encoding' => 'gzip',
      'user-agent' => user_agent,
      'content-length' => to_char_list(byte_size(body))}
    headers = Dict.merge(default_headers, headers)
    http_opts = [timeout: 5000]
    opts = [body_format: :binary]

    body = fn
      size when size < byte_size(body) ->
        new_size = min(size + @chunk, byte_size(body))
        chunk = new_size - size
        progress.(new_size)
        {:ok, [:binary.part(body, size, chunk)], new_size}
      _size ->
        :eof
    end

    request = {url, Map.to_list(headers), 'application/octet-stream', {body, 0}}

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
    Util.handle_hex_message(headers['x-hex-message'])

    if String.contains?(content_encoding, "gzip") do
      body = :zlib.gunzip(body)
    end

    if String.contains?(content_type, "application/vnd.hex+elixir") do
      body = Hex.Util.safe_deserialize_elixir(body)
    end

    {code, body}
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

  def auth(key: secret) do
    %{'authorization' => String.to_char_list(secret)}
  end

  def auth(info) do
    base64 = :base64.encode_to_string(info[:user] <> ":" <> info[:pass])
    %{'authorization' => 'Basic ' ++ base64}
  end
end
