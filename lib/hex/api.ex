defmodule Hex.API do
  alias Hex.API.Utils

  @request_timeout 25_000
  @erlang_vendor 'application/vnd.hex+erlang'

  def request(method, url, headers, body \\ nil)
  when (is_map(headers) or is_list(headers)) and (body == nil or is_map(body)) do
    default_headers = %{
      'accept' => @erlang_vendor,
      'accept-encoding' => 'gzip',
      'user-agent' => user_agent()}
    headers = Enum.into(headers, default_headers)

    http_opts = [relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    opts = [body_format: :binary]
    url = Hex.string_to_charlist(url)
    profile = Hex.State.fetch!(:httpc_profile)

    request =
      cond do
        body ->
          body = Hex.Utils.safe_serialize_erlang(body)
          {url, Map.to_list(headers), @erlang_vendor, body}
        method in [:put, :post] ->
          body = :erlang.term_to_binary(%{})
          {url, Map.to_list(headers), @erlang_vendor, body}
        true ->
          {url, Map.to_list(headers)}
      end

    retry(method, 2, fn ->
      case request_with_redirect(method, request, http_opts, opts, profile, 3) do
        {:ok, response} ->
          handle_response(response)
        {:error, reason} ->
          {:http_error, reason, []}
      end
    end)
  end

  def request_with_redirect(method, request, http_opts, opts, profile, times) do
    url = elem(request, 0)
    http_opts =
      http_opts
      |> Keyword.put(:ssl, Hex.API.SSL.ssl_opts(url))
      |> Keyword.put_new(:autoredirect, false)

    case :httpc.request(method, request, http_opts, opts, profile) do
      {:ok, response} ->
        case handle_redirect(response) do
          {:ok, location} when times > 0 ->
            request = update_request(request, location)
            request_with_redirect(method, request, http_opts, opts, profile, times - 1)
          {:ok, _location} ->
            Mix.raise "Too many redirects"
          :error ->
            {:ok, response}
        end
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp update_request({_url, headers, content_type, body}, new_url),
    do: {new_url, headers, content_type, body}
  defp update_request({_url, headers}, new_url),
    do: {new_url, headers}

  defp handle_redirect({{_version, code, _reason}, headers, _body})
  when code in [301, 302, 303, 307, 308] do
    headers = Enum.into(headers, %{})
    if location = headers['location'] do
      {:ok, location}
    else
      :error
    end
  end
  defp handle_redirect(_), do: :error

  @chunk 10_000

  def request_tar(url, headers, body, progress) do
    default_headers = %{
      'accept' => @erlang_vendor,
      'user-agent' => user_agent(),
      'content-length' => Hex.to_charlist(byte_size(body))}
    headers = Enum.into(headers, default_headers)
    http_opts = [relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    opts = [body_format: :binary]
    url = Hex.string_to_charlist(url)
    profile = Hex.State.fetch!(:httpc_profile)

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

    case request_with_redirect(:post, request, http_opts, opts, profile, 3) do
      {:ok, response} ->
        handle_response(response)
      {:error, reason} ->
        {:http_error, reason, []}
    end
  end

  defp handle_response({{_version, code, _reason}, headers, body}) do
    headers = Enum.into(headers, %{})
    Utils.handle_hex_message(headers['x-hex-message'])

    body =
      body
      |> unzip(headers)
      |> decode(headers)

    {code, body, headers}
  end

  defp unzip(body, headers) do
    content_encoding = List.to_string(headers['content-encoding'] || '')
    if String.contains?(content_encoding, "gzip") do
      :zlib.gunzip(body)
    else
      body
    end
  end

  defp decode(body, headers) do
    content_type = List.to_string(headers['content-type'] || '')
    erlang_vendor = List.to_string(@erlang_vendor)

    if String.contains?(content_type, erlang_vendor) do
      Hex.Utils.safe_deserialize_erlang(body)
    else
      body
    end
  end

  def user_agent do
    'Hex/#{Hex.version} (Elixir/#{System.version}) (OTP/#{Hex.Utils.otp_version})'
  end

  def repo_url(path) do
    base = Hex.State.fetch!(:repo) || Hex.State.fetch!(:mirror)
    base <> "/" <> path
  end

  def api_url(path) do
    Hex.State.fetch!(:api) <> "/" <> path
  end

  def auth(opts) do
    if key = opts[:key] do
      %{'authorization' => Hex.string_to_charlist(key)}
    else
      base64 = :base64.encode_to_string(opts[:user] <> ":" <> opts[:pass])
      %{'authorization' => 'Basic ' ++ base64}
    end
  end

  defp retry(:get, times, fun) do
    case fun.() do
      {:http_error, _, _} when times > 1 ->
        retry(:get, times - 1, fun)
      {:http_error, _, _} = error ->
        error
      other ->
        other
    end
  end
  defp retry(_method, _times, fun), do: fun.()
end
