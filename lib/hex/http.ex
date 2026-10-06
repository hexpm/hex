defmodule Hex.HTTP do
  @moduledoc false

  @behaviour :mix_hex_http

  @request_timeout 15_000
  @request_redirects 3
  @request_retries 2
  @chunk_size 10_000
  @unreachable_errors [:ehostunreach, :enetunreach, :eprotonosupport, :nxdomain]

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

    case run_request(method, url, headers, body, timeout, nil) do
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

    case run_request(method, url, headers, body, timeout, filename) do
      {:ok, status, headers_map, _nil} ->
        {:ok, {status, headers_map}}

      {:error, reason} ->
        _ = File.rm(filename)
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

  defp run_request(method, url, headers, body, timeout, filename) do
    # Start with IPv4 until a request fails with an unreachable-network error,
    # retry switches the address family for this and later requests.
    inet = Hex.State.get(:http_inet) || :inet

    retry(method, url, headers, body, timeout, inet, @request_retries, fn url, headers, inet ->
      redirect(method, url, headers, body, timeout, inet, @request_redirects, fn url,
                                                                                 headers,
                                                                                 inet ->
        timeout(timeout, fn ->
          do_request(method, url, headers, body, timeout, inet, filename)
        end)
      end)
    end)
  end

  ## Core request via Mint-backed pool

  defp do_request(method, url, headers, body, timeout, inet, filename) do
    connect_opts = build_connect_opts(url, inet, timeout)
    headers = put_userinfo_auth(headers, url)

    mint_method = method |> Atom.to_string() |> String.upcase()

    {mint_headers, mint_body} = build_mint_request(headers, body)

    pool_opts = [timeout: timeout, connect_opts: connect_opts]

    pool_result =
      case filename do
        nil ->
          Pool.request(url, mint_method, mint_headers, mint_body, pool_opts)

        filename when is_binary(filename) ->
          Pool.request_to_file(url, mint_method, mint_headers, mint_body, filename, pool_opts)
      end

    case pool_result do
      {:ok, status, resp_headers, resp_body} ->
        headers_map = headers_to_map(resp_headers)
        handle_hex_message(headers_map["x-hex-message"])
        Hex.Shell.debug("Hex.HTTP.request(#{inspect(method)}, #{inspect(url)}) => #{status}")
        {:ok, status, headers_map, maybe_unzip(resp_body, headers_map)}

      {:error, reason} ->
        Hex.Shell.debug(
          "Hex.HTTP.request(#{inspect(method)}, #{inspect(url)}) => #{inspect(reason, limit: :infinity, pretty: true)}"
        )

        {:error, reason}
    end
  end

  defp maybe_unzip(nil, _headers), do: nil
  defp maybe_unzip(body, headers) when is_binary(body), do: unzip(body, headers)

  defp build_mint_request(headers, body) do
    base_headers = Enum.map(headers, fn {k, v} -> {to_string(k), to_string(v)} end)

    case body do
      {content_type, binary} when is_binary(binary) ->
        ct = if is_binary(content_type), do: content_type, else: to_string(content_type)
        {[{"content-type", ct} | base_headers], binary}

      {content_type, {fun, initial_offset}} when is_function(fun, 1) ->
        # Progress-callback streaming: send headers first, then feed the
        # producer function chunk-by-chunk through the pool so the callback
        # fires as bytes actually go out on the wire (for `mix hex.publish`
        # progress output).
        ct = if is_binary(content_type), do: content_type, else: to_string(content_type)
        {[{"content-type", ct} | base_headers], {:stream, fun, initial_offset}}

      nil ->
        {base_headers, nil}

      :undefined ->
        {base_headers, nil}
    end
  end

  # A header sent more than once keeps its last value
  defp headers_to_map(headers) when is_list(headers) do
    Map.new(headers, fn {name, value} ->
      {String.downcase(to_string(name)), to_string(value)}
    end)
  end

  # Credentials in the URL take precedence over other authorization, they're
  # only sent to the URL they're part of and not to redirects
  defp put_userinfo_auth(headers, url) do
    case URI.parse(url).userinfo do
      nil ->
        headers

      userinfo ->
        {user, password} = split_userinfo(userinfo)
        credentials = Base.encode64(percent_decode(user) <> ":" <> percent_decode(password))
        Map.put(headers, "authorization", "Basic " <> credentials)
    end
  end

  defp split_userinfo(userinfo) do
    case String.split(userinfo, ":", parts: 2) do
      [user, password] -> {user, password}
      [user] -> {user, ""}
    end
  end

  defp percent_decode(string) do
    URI.decode(string)
  rescue
    ArgumentError -> string
  end

  defp build_connect_opts(url, inet, timeout) do
    uri = URI.parse(url)
    socket_opts = socket_opts(inet, timeout)

    transport_opts =
      case uri.scheme do
        "https" -> socket_opts ++ Hex.HTTP.SSL.ssl_opts(url)
        _ -> socket_opts
      end

    [transport_opts: transport_opts] ++ proxy_connect_opts(uri, socket_opts, timeout)
  end

  # Mint's transport flags for the address family, and the timeout for
  # connecting and the TLS handshake
  defp socket_opts(:inet, timeout), do: [inet4: true, inet6: false, timeout: timeout]
  defp socket_opts(:inet6, timeout), do: [inet4: false, inet6: true, timeout: timeout]

  ## Retry

  defp retry(method, url, headers, body, timeout, inet, times, fun) do
    case fun.(url, headers, inet) do
      {:error, reason} = error ->
        if retryable?(method, reason) and times > 0 do
          new_inet = fallback_inet(reason, inet)
          retry(method, url, headers, body, timeout, new_inet, times - 1, fun)
        else
          error
        end

      other ->
        other
    end
  end

  # Requests that failed before the server could process them are retried for
  # every method, other failures only for GET requests
  @doc false
  def retryable?(method, reason) do
    unprocessed?(reason) or (method == :get and retryable_get?(reason))
  end

  defp unprocessed?(%Hex.Mint.TransportError{reason: reason}),
    do: reason in [:econnrefused | @unreachable_errors]

  defp unprocessed?(%Hex.Mint.HTTPError{reason: :unprocessed}), do: true

  defp unprocessed?(%Hex.Mint.HTTPError{reason: {:server_closed_request, :refused_stream}}),
    do: true

  defp unprocessed?(_), do: false

  defp retryable_get?(%Hex.Mint.TransportError{reason: reason}),
    do: reason in [:closed, :timeout]

  defp retryable_get?(_), do: false

  defp fallback_inet(%Hex.Mint.TransportError{reason: reason}, inet)
       when reason in @unreachable_errors do
    new_inet =
      case inet do
        :inet -> :inet6
        :inet6 -> :inet
      end

    Hex.State.put(:http_inet, new_inet)
    new_inet
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
    proxy_connect_opts(URI.parse(url), socket_opts(:inet, @request_timeout), @request_timeout)
  end

  defp proxy_connect_opts(%URI{host: host, scheme: scheme}, socket_opts, timeout) do
    if host_in_no_proxy?(host, no_proxy()) do
      []
    else
      case proxy_uri_for(scheme) do
        %URI{host: phost, port: pport} = proxy when not is_nil(phost) and not is_nil(pport) ->
          proxy_opts = [transport_opts: socket_opts, tunnel_timeout: timeout]

          proxy_headers =
            case proxy.userinfo do
              nil ->
                []

              userinfo ->
                {user, password} = split_userinfo(userinfo)
                encoded = Base.encode64(user <> ":" <> password)
                [proxy_headers: [{"proxy-authorization", "Basic #{encoded}"}]]
            end

          [proxy: {:http, phost, pport, proxy_opts}] ++ proxy_headers

        _ ->
          []
      end
    end
  end

  defp proxy_uri_for("http"), do: parse_proxy(Hex.State.fetch!(:http_proxy))

  # HTTPS requests go through http_proxy when https_proxy isn't set
  defp proxy_uri_for("https") do
    parse_proxy(Hex.State.fetch!(:https_proxy) || Hex.State.fetch!(:http_proxy))
  end

  defp proxy_uri_for(_), do: nil

  defp parse_proxy(nil), do: nil
  defp parse_proxy(proxy), do: URI.parse(proxy)

  defp no_proxy do
    (Hex.State.fetch!(:no_proxy) || "")
    |> String.split(",", trim: true)
    |> Enum.map(&(&1 |> String.trim() |> String.downcase()))
    |> Enum.reject(&(&1 == ""))
  end

  defp host_in_no_proxy?(host, patterns) do
    host = String.downcase(host)
    Enum.any?(patterns, &no_proxy_match?(host, &1))
  end

  # `*` matches every host, `*.example.com` and `.example.com` match
  # example.com and its subdomains, an IP address prefix such as `10.` or
  # `fd00:` matches the addresses that start with it, and any other entry
  # matches that host and its subdomains
  defp no_proxy_match?(_host, "*"), do: true
  defp no_proxy_match?(host, "*." <> domain), do: domain_match?(host, domain)
  defp no_proxy_match?(host, "." <> domain), do: domain_match?(host, domain)

  defp no_proxy_match?(host, pattern) do
    if address_prefix?(pattern) do
      String.starts_with?(host, pattern)
    else
      domain_match?(host, pattern)
    end
  end

  defp domain_match?(host, domain) do
    host == domain or String.ends_with?(host, "." <> domain)
  end

  defp address_prefix?(pattern) do
    String.contains?(pattern, ":") or
      pattern |> String.to_charlist() |> Enum.all?(&(&1 in ?0..?9 or &1 == ?.))
  end

  ## Hex messages

  def handle_hex_message(nil), do: :ok

  def handle_hex_message(header) do
    {message, level} = IO.iodata_to_binary(header) |> parse_hex_message
    message = Hex.Utils.escape_terminal(message)

    case level do
      "warn" -> Hex.Shell.info("API warning: " <> message)
      "fatal" -> Hex.Shell.error("API error: " <> message)
      _ -> :ok
    end
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
