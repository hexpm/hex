defmodule Hex.HTTP do
  @moduledoc false

  @behaviour :mix_hex_http

  @request_timeout 15_000
  @request_redirects 3
  @request_retries 2
  @chunk_size 10_000

  alias Hex.HTTP.Pool

  @spec config() :: :mix_hex_core.config()
  def config do
    %{
      :mix_hex_core.default_config()
      | http_adapter: {__MODULE__, %{}}
    }
  end

  # Keep old signatures for backward compatibility
  def request(method, url, headers, body) do
    request(method, url, headers, body, %{})
  end

  def request(method, url, headers, body, opts) when is_list(opts) do
    adapter_config = Keyword.get(opts, :adapter_config, %{})
    request(method, url, headers, body, adapter_config)
  end

  @impl :mix_hex_http
  def request(method, url, headers, body, adapter_config) when is_map(adapter_config) do
    {method, url, headers, body, timeout} =
      prepare_request(method, url, headers, body, adapter_config)

    Hex.Shell.debug("Hex.HTTP.request(#{inspect(method)}, #{inspect(url)})")

    case run_request(method, url, headers, body, timeout) do
      {:ok, status, headers_map, resp_body} ->
        {:ok, {status, headers_map, resp_body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @impl :mix_hex_http
  def request_to_file(method, url, headers, body, filename, adapter_config)
      when is_map(adapter_config) do
    {method, url, headers, body, timeout} =
      prepare_request(method, url, headers, body, adapter_config)

    Hex.Shell.debug("Hex.HTTP.request_to_file(#{inspect(method)}, #{inspect(url)})")

    case run_request(method, url, headers, body, timeout) do
      {:ok, status, headers_map, resp_body} ->
        case File.write(filename, resp_body) do
          :ok -> {:ok, {status, headers_map}}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp prepare_request(method, url, headers, body, adapter_config) do
    method = if is_binary(method), do: String.to_atom(method), else: method
    url = if is_binary(url), do: url, else: to_string(url)
    headers = if is_map(headers), do: headers, else: Map.new(headers)

    headers = add_basic_auth_via_netrc(headers, url)

    timeout =
      adapter_config[:timeout] ||
        Hex.State.fetch!(:http_timeout, fn val -> if is_integer(val), do: val * 1000 end) ||
        @request_timeout

    progress_callback = Map.get(adapter_config, :progress_callback)
    {body, extra_headers} = wrap_body_with_progress(body, progress_callback)
    headers = Map.merge(headers, extra_headers)

    {method, url, headers, body, timeout}
  end

  defp run_request(method, url, headers, body, timeout) do
    # Start with :inet (IPv4); retry will swap to :inet6 on network failures.
    inet = :inet

    retry(method, url, headers, body, timeout, inet, @request_retries, fn url, headers, inet ->
      redirect(method, url, headers, body, timeout, inet, @request_redirects, fn url,
                                                                                 headers,
                                                                                 inet ->
        timeout(timeout, fn ->
          do_request(method, url, headers, body, timeout, inet)
        end)
      end)
    end)
  end

  ## Core request via Mint-backed pool

  defp do_request(method, url, headers, body, timeout, inet) do
    connect_opts = build_connect_opts(url, inet)

    mint_method = method |> Atom.to_string() |> String.upcase()

    {mint_headers, mint_body} = build_mint_request(headers, body)

    pool_opts = [timeout: timeout, connect_opts: connect_opts]

    case Pool.request(url, mint_method, mint_headers, mint_body, pool_opts) do
      {:ok, status, resp_headers, resp_body} ->
        headers_map = headers_to_map(resp_headers)
        handle_hex_message(headers_map["x-hex-message"])
        Hex.Shell.debug("Hex.HTTP.request(#{inspect(method)}, #{inspect(url)}) => #{status}")
        {:ok, status, headers_map, unzip(resp_body, headers_map)}

      {:error, reason} ->
        Hex.Shell.debug(
          "Hex.HTTP.request(#{inspect(method)}, #{inspect(url)}) => #{inspect(reason, limit: :infinity, pretty: true)}"
        )

        {:error, reason}
    end
  end

  defp build_mint_request(headers, body) do
    base_headers = Enum.map(headers, fn {k, v} -> {to_string(k), to_string(v)} end)

    case body do
      {content_type, binary} when is_binary(binary) ->
        ct = if is_binary(content_type), do: content_type, else: to_string(content_type)
        {[{"content-type", ct} | base_headers], binary}

      {content_type, {fun, _initial} = _streamed} when is_function(fun, 1) ->
        # Progress-callback streaming: collect chunks eagerly for now.
        # The pool could support :stream in the future; keeping this simple
        # since progress is only used for `mix hex.publish`.
        ct = if is_binary(content_type), do: content_type, else: to_string(content_type)
        binary = collect_chunks(fun, 0, [])
        {[{"content-type", ct} | base_headers], binary}

      nil ->
        {base_headers, nil}

      :undefined ->
        {base_headers, nil}
    end
  end

  defp collect_chunks(fun, offset, acc) do
    case fun.(offset) do
      :eof -> IO.iodata_to_binary(Enum.reverse(acc))
      {:ok, chunk, new_offset} -> collect_chunks(fun, new_offset, [chunk | acc])
    end
  end

  defp headers_to_map(headers) when is_list(headers) do
    Enum.reduce(headers, %{}, fn {name, value}, acc ->
      name = String.downcase(to_string(name))
      value = to_string(value)

      Map.update(acc, name, value, fn existing -> existing <> ", " <> value end)
    end)
  end

  defp build_connect_opts(url, inet) do
    uri = URI.parse(url)

    # Convert our :inet / :inet6 marker into Mint's transport flags.
    inet_opts =
      case inet do
        :inet -> [inet4: true, inet6: false]
        :inet6 -> [inet4: false, inet6: true]
      end

    transport_opts =
      case uri.scheme do
        "https" -> inet_opts ++ Hex.HTTP.SSL.ssl_opts(url)
        _ -> inet_opts
      end

    [transport_opts: transport_opts] ++ proxy_connect_opts(uri)
  end

  ## Retry

  defp retry(:get, url, headers, body, timeout, inet, times, fun) do
    case fun.(url, headers, inet) do
      {:error, reason} = error ->
        if retryable?(reason) and times > 0 do
          new_inet = fallback_inet(reason, inet)
          retry(:get, url, headers, body, timeout, new_inet, times - 1, fun)
        else
          error
        end

      other ->
        other
    end
  end

  defp retry(_method, url, headers, _body, _timeout, inet, _times, fun) do
    fun.(url, headers, inet)
  end

  defp retryable?(%Hex.Mint.TransportError{reason: :closed}), do: true
  defp retryable?(%Hex.Mint.TransportError{reason: :timeout}), do: true
  defp retryable?(%Hex.Mint.TransportError{reason: :econnrefused}), do: true
  defp retryable?(%Hex.Mint.TransportError{reason: :ehostunreach}), do: true
  defp retryable?(%Hex.Mint.TransportError{reason: :enetunreach}), do: true
  defp retryable?(%Hex.Mint.TransportError{reason: :eprotonosupport}), do: true
  defp retryable?(%Hex.Mint.TransportError{reason: :nxdomain}), do: true
  defp retryable?(:disconnected), do: true
  defp retryable?(:socket_closed_remotely), do: true
  defp retryable?(_), do: false

  defp fallback_inet(%Hex.Mint.TransportError{reason: reason}, inet)
       when reason in [:ehostunreach, :enetunreach, :eprotonosupport, :nxdomain] do
    case inet do
      :inet -> :inet6
      :inet6 -> :inet
    end
  end

  defp fallback_inet(_reason, inet), do: inet

  ## Redirect

  defp redirect(method, url, headers, body, timeout, inet, times, fun) do
    case fun.(url, headers, inet) do
      {:ok, code, resp_headers, _resp_body} = resp ->
        case handle_redirect(code, resp_headers) do
          {:ok, location} when times > 0 ->
            location = resolve_location(url, location)
            redirect(method, location, headers, body, timeout, inet, times - 1, fun)

          {:ok, _} ->
            Mix.raise("Too many redirects")

          :error ->
            resp
        end

      other ->
        other
    end
  end

  defp handle_redirect(code, headers) when code in [301, 302, 303, 307, 308] do
    case headers["location"] do
      nil -> :error
      loc -> {:ok, loc}
    end
  end

  defp handle_redirect(_, _), do: :error

  defp resolve_location(base, location) do
    case URI.parse(location) do
      %URI{host: nil} = uri ->
        %URI{} = base_uri = URI.parse(base)
        URI.to_string(%{base_uri | path: uri.path, query: uri.query, fragment: uri.fragment})

      _ ->
        location
    end
  end

  ## Timeout

  defp timeout(timeout, fun) do
    task = Task.async(fun)

    try do
      Task.await(task, timeout)
    catch
      :exit, {:timeout, _} ->
        Process.unlink(task.pid)
        Process.exit(task.pid, :kill)
        {:error, :timeout}
    end
  end

  ## Response helpers

  defp unzip(body, headers) do
    encoding = headers["content-encoding"] || ""

    if String.contains?(encoding, "gzip") do
      :zlib.gunzip(body)
    else
      body
    end
  end

  ## Proxy

  def proxy_config(url) do
    uri = URI.parse(url)
    proxy_connect_opts(uri)
  end

  defp proxy_connect_opts(%URI{host: host, scheme: scheme} = uri) do
    no_proxy = no_proxy()

    if host_in_no_proxy?(host, no_proxy) do
      []
    else
      case proxy_uri_for(scheme) do
        nil ->
          []

        %URI{host: phost, port: pport} = proxy when not is_nil(phost) and not is_nil(pport) ->
          proxy_opts =
            case proxy.userinfo do
              nil ->
                []

              userinfo ->
                encoded = Base.encode64(userinfo)
                [proxy_headers: [{"proxy-authorization", "Basic #{encoded}"}]]
            end

          [proxy: {:http, to_charlist(phost), pport, proxy_opts}]

        _ ->
          []
      end
      |> Kernel.++(maybe_proxy_ignored(uri))
    end
  end

  defp proxy_connect_opts(_), do: []

  defp maybe_proxy_ignored(_), do: []

  defp proxy_uri_for("http") do
    case Hex.State.fetch!(:http_proxy) do
      nil -> nil
      str -> URI.parse(str)
    end
  end

  defp proxy_uri_for("https") do
    case Hex.State.fetch!(:https_proxy) do
      nil -> nil
      str -> URI.parse(str)
    end
  end

  defp proxy_uri_for(_), do: nil

  defp no_proxy do
    (Hex.State.fetch!(:no_proxy) || "")
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
  end

  defp host_in_no_proxy?(_host, []), do: false

  defp host_in_no_proxy?(host, patterns) do
    Enum.any?(patterns, fn pattern ->
      pattern = String.trim_leading(pattern, ".")
      host == pattern or String.ends_with?(host, "." <> pattern)
    end)
  end

  ## Hex messages

  def handle_hex_message(nil), do: :ok

  def handle_hex_message(header) when is_binary(header) do
    {message, level} = parse_hex_message(header)

    case level do
      "warn" -> Hex.Shell.info("API warning: " <> message)
      "fatal" -> Hex.Shell.error("API error: " <> message)
      _ -> :ok
    end
  end

  def handle_hex_message(header) when is_list(header) do
    handle_hex_message(:binary.list_to_bin(header))
  end

  @space [?\s, ?\t]

  defp parse_hex_message(message) do
    {message, rest} = skip_ws(message) |> quoted
    level = skip_ws(rest) |> opt_level
    {message, level}
  end

  defp skip_ws(<<char, rest::binary>>) when char in @space, do: skip_ws(rest)
  defp skip_ws(rest), do: rest

  defp skip_trail_ws(<<char, rest::binary>>, str, ws) when char in @space do
    skip_trail_ws(rest, str, <<ws::binary, char>>)
  end

  defp skip_trail_ws(<<char, rest::binary>>, str, ws) do
    skip_trail_ws(rest, <<str::binary, ws::binary, char>>, "")
  end

  defp skip_trail_ws("", str, _ws), do: str

  defp quoted("\"" <> rest), do: do_quoted(rest, "")

  defp do_quoted("\"" <> rest, acc), do: {acc, rest}
  defp do_quoted(<<char, rest::binary>>, acc), do: do_quoted(rest, <<acc::binary, char>>)

  defp opt_level(";" <> rest), do: do_level(rest)
  defp opt_level(_), do: nil

  defp do_level(rest) do
    "level" <> rest = skip_ws(rest)
    "=" <> rest = skip_ws(rest)

    rest
    |> skip_ws()
    |> skip_trail_ws("", "")
  end

  ## Auth

  defp add_basic_auth_via_netrc(%{"authorization" => _} = headers, _url), do: headers

  defp add_basic_auth_via_netrc(%{} = headers, url) do
    url = URI.parse(url)

    case Hex.Netrc.lookup(url.host) do
      {:ok, %{username: username, password: password}} ->
        base64 = :base64.encode_to_string("#{username}:#{password}")
        Map.put(headers, "authorization", "Basic #{base64}")

      _ ->
        headers
    end
  end

  ## Progress callback

  defp wrap_body_with_progress(body, progress_callback) do
    case body do
      {content_type, binary_body}
      when is_binary(binary_body) and is_function(progress_callback, 1) ->
        total_size = byte_size(binary_body)

        body_fn = fn
          size when size < total_size ->
            new_size = min(size + @chunk_size, total_size)
            chunk = new_size - size
            progress_callback.(new_size)
            {:ok, :binary.part(binary_body, size, chunk), new_size}

          _size ->
            :eof
        end

        body_result = {content_type, {body_fn, 0}}
        headers = %{"content-length" => Integer.to_string(total_size)}
        {body_result, headers}

      _other ->
        {body, %{}}
    end
  end
end
