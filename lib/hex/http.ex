defmodule Hex.HTTP do
  @max_timeout 600_000
  @request_timeout 15_000
  @chunk_size 1460
  @request_redirects 3
  @request_retries 2

  def request(method, url, headers, body, opts \\ []) do
    headers = build_headers(headers, body)
    timeout = opts[:timeout] || Hex.State.fetch!(:http_timeout) || @request_timeout
    http_opts = build_http_opts(url)
    opts = [body_format: :binary, sync: false, stream: :self]
    profile = Hex.State.fetch!(:httpc_profile)
    watch_task = Task.async(fn -> watcher(timeout) end)
    request = build_request(url, headers, body, watch_task)

    {:ok, request_task} = Task.start_link(fn ->
      do_request(method, request, watch_task, fn request ->
        :httpc.request(method, request, http_opts, opts, profile)
      end)
    end)

    response = Task.await(watch_task, @max_timeout)
    Process.unlink(request_task)
    Process.exit(request_task, :kill)
    handle_response(response)
  end

  defp watcher(timeout) do
    receive do
      :progress ->
        watcher(timeout)
      {:result, result} ->
        result
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp do_request(method, request, watcher, fun) do
    result =
      retry(method, request, @request_retries, fn request ->
        redirect(request, @request_redirects, fn request ->
          stream(watcher, fn -> fun.(request) end)
        end)
      end)

    send(watcher.pid, {:result, result})
  end

  defp build_headers(headers, body) do
    %{'user-agent' => user_agent()}
    |> Map.merge(content_length(body))
    |> Map.merge(Enum.into(headers, %{}))
  end

  defp content_length({_content_type, body, _fun}) do
    length = body |> byte_size() |> Hex.to_charlist()
    %{'content-length' => length}
  end
  defp content_length(nil), do: %{}

  defp build_http_opts(url) do
    [
      relaxed: true,
      ssl: Hex.HTTP.SSL.ssl_opts(url),
      autoredirect: false
    ] ++ proxy_config(url)
  end

  defp build_request(url, headers, body, watcher) do
    url = Hex.string_to_charlist(url)
    headers = Map.to_list(headers)

    case body do
      {content_type, body, fun} ->
        {url, headers, content_type, build_body(body, fun, watcher)}
      nil ->
        {url, headers}
    end
  end

  defp build_body(body, fun, watcher) do
    body = fn
      size when size < byte_size(body) ->
        new_size = min(size + @chunk_size, byte_size(body))
        chunk = new_size - size
        fun.(new_size)
        send(watcher.pid, :progress)
        {:ok, :binary.part(body, size, chunk), new_size}
      _size ->
        send(watcher.pid, :progress)
        :eof
    end

    {body, 0}
  end

  defp retry(:get, request, times, fun) do
    result =
      case fun.(request) do
        {:http_error, _, _} = error ->
          {:retry, error}
        {:error, :socket_closed_remotely} = error ->
          {:retry, error}
        other ->
          {:noretry, other}
      end

    case result do
      {:retry, _} when times > 0 ->
        retry(:get, request, times - 1, fun)
      {_other, result} ->
        result
    end
  end
  defp retry(_method, request, _times, fun), do: fun.(request)

  defp redirect(request, times, fun) do
    case fun.(request) do
      {:ok, response} ->
        case handle_redirect(response) do
          {:ok, location} when times > 0 ->
            request
            |> update_request(location)
            |> redirect(times - 0, fun)

          {:ok, _location} ->
            Mix.raise "Too many redirects"

          :error ->
            {:ok, response}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_redirect({{_version, code, _reason}, headers, _body})
       when code in [301, 302, 303, 307, 308] do
    headers = Enum.into(headers, %{})

    if location = headers['location'] do
      {:ok, location}
    else
      :error
    end
  end
  defp handle_redirect(_) do
    :error
  end

  defp update_request({_url, headers, content_type, body}, new_url) do
    {new_url, headers, content_type, body}
  end
  defp update_request({_url, headers}, new_url) do
    {new_url, headers}
  end

  defp stream(watcher, fun) do
    case fun.() do
      {:ok, _} ->
        stream_response(watcher, :start)
      other ->
        other
    end
  end

  defp stream_response(watcher, :start) do
    receive do
      {:http, {_request_id, :stream_start, headers}} ->
        stream_response(watcher, {headers, ""})
      {:http, {_request_id, {status, headers, body}}} ->
        {:ok, {status, headers, body}}
      {:http, {_request_id, {:error, reason}}} ->
        {:error, reason}
    end
  end

  defp stream_response(watcher, {headers, body}) do
    send(watcher.pid, :progress)

    receive do
      {:http, {_request_id, :stream, body_part}} ->
        stream_response(watcher, {headers, [body|body_part]})
      {:http, {_request_id, :stream_end, headers_part}} ->
        {:ok, {200, headers ++ headers_part, IO.iodata_to_binary(body)}}
      {:http, {_request_id, {:error, reason}}} ->
        {:error, reason}
    end
  end

  defp handle_response({:ok, {{_version, code, _reason}, headers, body}}) do
    handle_response({:ok, {code, headers, body}})
  end
  defp handle_response({:ok, {code, headers, body}}) do
    headers = Enum.into(headers, %{})
    handle_hex_message(headers['x-hex-message'])
    {:ok, {code, unzip(body, headers), headers}}
  end
  defp handle_response({:error, term}) do
    {:error, term}
  end

  defp unzip(body, headers) do
    content_encoding = List.to_string(headers['content-encoding'] || '')

    if String.contains?(content_encoding, "gzip") do
      :zlib.gunzip(body)
    else
      body
    end
  end

  def proxy_config(url) do
    {http_proxy, https_proxy} = proxy_setup()
    proxy_auth(URI.parse(url), http_proxy, https_proxy)
  end

  defp proxy_setup do
    http_proxy = (proxy = Hex.State.get(:http_proxy)) && proxy(:http, proxy)
    https_proxy = (proxy = Hex.State.get(:https_proxy)) && proxy(:https, proxy)
    {http_proxy, https_proxy}
  end

  defp proxy(scheme, proxy) do
    uri = URI.parse(proxy)

    if uri.host && uri.port do
      host = Hex.string_to_charlist(uri.host)
      :httpc.set_options([{proxy_scheme(scheme), {{host, uri.port}, []}}], :hex)
    end

    uri
  end

  defp proxy_scheme(scheme) do
    case scheme do
      :http -> :proxy
      :https -> :https_proxy
    end
  end

  defp proxy_auth(%URI{scheme: "http"}, http_proxy, _https_proxy) do
    proxy_auth(http_proxy)
  end
  defp proxy_auth(%URI{scheme: "https"}, _http_proxy, https_proxy) do
    proxy_auth(https_proxy)
  end

  defp proxy_auth(nil) do
    []
  end
  defp proxy_auth(%URI{userinfo: nil}) do
    []
  end
  defp proxy_auth(%URI{userinfo: auth}) do
    destructure [user, pass], String.split(auth, ":", parts: 2)

    user = Hex.string_to_charlist(user)
    pass = Hex.string_to_charlist(pass || "")
    [proxy_auth: {user, pass}]
  end

  defp user_agent do
    'Hex/#{Hex.version} (Elixir/#{System.version}) (OTP/#{Hex.Utils.otp_version})'
  end

  def handle_hex_message(nil) do
    :ok
  end
  def handle_hex_message(header) do
    {message, level} = :binary.list_to_bin(header) |> parse_hex_message
    case level do
      "warn"  -> Hex.Shell.info "API warning: " <> message
      "fatal" -> Hex.Shell.error "API error: " <> message
      _       -> :ok
    end
  end

  @space [?\s, ?\t]

  defp parse_hex_message(message) do
    {message, rest} = skip_ws(message) |> quoted
    level = skip_ws(rest) |> opt_level
    {message, level}
  end

  defp skip_ws(<<char, rest :: binary>>) when char in @space, do: skip_ws(rest)
  defp skip_ws(rest), do: rest

  defp skip_trail_ws(<<char, rest :: binary>>, str, ws) when char in @space do
    skip_trail_ws(rest, str, <<ws :: binary, char>>)
  end
  defp skip_trail_ws(<<char, rest :: binary>>, str, ws) do
    skip_trail_ws(rest, <<str :: binary, ws :: binary, char>>, "")
  end
  defp skip_trail_ws("", str, _ws) do
    str
  end

  defp quoted("\"" <> rest), do: do_quoted(rest, "")

  defp do_quoted("\"" <> rest, acc), do: {acc, rest}
  defp do_quoted(<<char, rest :: binary>>, acc), do: do_quoted(rest, <<acc :: binary, char>>)

  defp opt_level(";" <> rest), do: do_level(rest)
  defp opt_level(_), do: nil

  defp do_level(rest) do
    "level" <> rest = skip_ws(rest)
    "=" <> rest = skip_ws(rest)

    rest
    |> skip_ws()
    |> skip_trail_ws("", "")
  end
end
