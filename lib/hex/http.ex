defmodule Hex.HTTP do
  @moduledoc false

  @behaviour :mix_hex_http

  @request_timeout 15_000
  @request_redirects 3
  @request_retries 2
  @chunk_size 10_000

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
    # Convert method to atom if it's not already
    method = if is_binary(method), do: String.to_atom(method), else: method
    # Convert URL to string if it's binary
    url = if is_binary(url), do: url, else: to_string(url)

    # Convert headers from map to our format
    headers = if is_map(headers), do: headers, else: Map.new(headers)

    Hex.Shell.debug("Hex.HTTP.request(#{inspect(method)}, #{inspect(url)})")

    headers = add_basic_auth_via_netrc(headers, url)

    timeout =
      adapter_config[:timeout] ||
        Hex.State.fetch!(:http_timeout, fn val -> if is_integer(val), do: val * 1000 end) ||
        @request_timeout

    # Handle progress callback for uploads
    progress_callback = Map.get(adapter_config, :progress_callback)
    {body, extra_headers} = wrap_body_with_progress(body, progress_callback)
    headers = Map.merge(headers, extra_headers)

    # Work around httpc bug: disable connection reuse when using Expect: 100-continue
    # httpc doesn't properly handle connection state when receiving final status (401)
    # instead of 100 Continue response
    headers =
      if headers["expect"] == "100-continue" do
        Map.put(headers, "connection", "close")
      else
        headers
      end

    http_opts = build_http_opts(url, timeout)
    opts = [body_format: :binary]
    request = build_request(url, headers, body)
    profile = Hex.State.fetch!(:httpc_profile)

    result =
      retry(method, request, http_opts, @request_retries, profile, fn request, http_opts ->
        redirect(request, http_opts, @request_redirects, fn request, http_opts ->
          timeout(request, http_opts, timeout, fn request, http_opts ->
            :httpc.request(method, request, http_opts, opts, profile)
            |> handle_response(method, url)
          end)
        end)
      end)

    # Convert to hex_core expected format
    case result do
      {:ok, status, headers, body} ->
        # Convert headers to map with binary keys/values for hex_core
        headers = Map.new(headers, fn {k, v} -> {to_string(k), to_string(v)} end)
        {:ok, {status, headers, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp fallback(:inet), do: :inet6
  defp fallback(:inet6), do: :inet

  defp build_http_opts(url, timeout) do
    [
      relaxed: true,
      timeout: timeout,
      ssl: Hex.HTTP.SSL.ssl_opts(url),
      autoredirect: false
    ] ++ proxy_config(url)
  end

  defp build_request(url, headers, body) do
    url = String.to_charlist(url)
    headers = Enum.map(headers, &encode_header/1)

    case body do
      {content_type, body} ->
        # content_type might already be a charlist from hex_core
        content_type =
          if is_binary(content_type) do
            String.to_charlist(content_type)
          else
            content_type
          end

        {url, headers, content_type, body}

      nil ->
        {url, headers}

      :undefined ->
        {url, headers}
    end
  end

  defp encode_header({name, value}) do
    {String.to_charlist(name), String.to_charlist(value)}
  end

  defp retry(:get, request, http_opts, times, profile, fun) do
    result =
      case fun.(request, http_opts) do
        {:http_error, _, _} = error ->
          {:retry, error}

        {:error, :socket_closed_remotely} = error ->
          {:retry, error}

        {:error, {:failed_connect, [{:to_address, to_addr}, {inet, inet_l, reason}]}}
        when inet in [:inet, :inet6] and
               reason in [:ehostunreach, :enetunreach, :eprotonosupport, :nxdomain] ->
          :httpc.set_options([ipfamily: fallback(inet)], profile)
          {:retry, {:error, {:failed_connect, [{:to_address, to_addr}, {inet, inet_l, reason}]}}}

        other ->
          {:noretry, other}
      end

    case result do
      {:retry, _} when times > 0 ->
        retry(:get, request, http_opts, times - 1, profile, fun)

      {_other, result} ->
        result
    end
  end

  defp retry(_method, request, http_opts, _times, _profile, fun), do: fun.(request, http_opts)

  defp redirect(request, http_opts, times, fun) do
    case fun.(request, http_opts) do
      {:ok, code, headers, body} ->
        case handle_redirect(code, headers) do
          {:ok, location} when times > 0 ->
            ssl_opts = Hex.HTTP.SSL.ssl_opts(to_string(location))
            http_opts = Keyword.put(http_opts, :ssl, ssl_opts)

            request
            |> update_request(location)
            |> redirect(http_opts, times - 0, fun)

          {:ok, _location} ->
            Mix.raise("Too many redirects")

          :error ->
            {:ok, code, headers, body}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_redirect(code, headers)
       when code in [301, 302, 303, 307, 308] do
    if location = headers["location"] do
      {:ok, location}
    else
      :error
    end
  end

  defp handle_redirect(_, _) do
    :error
  end

  defp update_request({_url, headers, content_type, body}, new_url) do
    {new_url, headers, content_type, body}
  end

  defp update_request({_url, headers}, new_url) do
    {new_url, headers}
  end

  defp timeout(request, http_opts, timeout, fun) do
    Task.async(fn -> fun.(request, http_opts) end)
    |> task_await(:timeout, timeout)
  end

  defp task_await(%Task{ref: ref, pid: pid} = task, reason, timeout) do
    receive do
      {^ref, result} ->
        Process.demonitor(ref, [:flush])
        result

      {:DOWN, ^ref, _, _, _reason} ->
        {:error, :timeout}
    after
      timeout ->
        Process.unlink(pid)
        Process.exit(pid, reason)
        task_await(task, :kill, timeout)
    end
  end

  defp handle_response({:ok, {{_version, code, _reason}, headers, body}}, method, url) do
    Hex.Shell.debug("Hex.HTTP.request(#{inspect(method)}, #{inspect(url)}) => #{code}")

    headers = Map.new(headers, &decode_header/1)
    handle_hex_message(headers["x-hex-message"])
    {:ok, code, headers, unzip(body, headers)}
  end

  defp handle_response({:error, term}, method, url) do
    Hex.Shell.debug(
      "Hex.HTTP.request(#{inspect(method)}, #{inspect(url)}) => #{inspect(term, limit: :infinity, pretty: true)}"
    )

    {:error, term}
  end

  defp decode_header({name, value}) do
    {List.to_string(name), List.to_string(value)}
  end

  defp unzip(body, headers) do
    content_encoding = headers["content-encoding"] || ""

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
    no_proxy = no_proxy()
    http_proxy = (proxy = Hex.State.fetch!(:http_proxy)) && proxy(:http, proxy, no_proxy)
    https_proxy = (proxy = Hex.State.fetch!(:https_proxy)) && proxy(:https, proxy, no_proxy)
    {http_proxy, https_proxy}
  end

  defp proxy(scheme, proxy, no_proxy) do
    uri = URI.parse(proxy)

    if uri.host && uri.port do
      host = String.to_charlist(uri.host)
      :httpc.set_options([{proxy_scheme(scheme), {{host, uri.port}, no_proxy}}], :hex)
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

    user = String.to_charlist(user)
    pass = String.to_charlist(pass || "")
    [proxy_auth: {user, pass}]
  end

  defp no_proxy() do
    (Hex.State.fetch!(:no_proxy) || "")
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&normalize_no_proxy_domain_desc/1)
    |> Enum.map(&String.to_charlist/1)
  end

  defp normalize_no_proxy_domain_desc(<<".", rest::binary>>) do
    "*." <> rest
  end

  defp normalize_no_proxy_domain_desc(host) do
    host
  end

  def handle_hex_message(nil) do
    :ok
  end

  def handle_hex_message(header) do
    {message, level} = :binary.list_to_bin(header) |> parse_hex_message

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

  defp skip_trail_ws("", str, _ws) do
    str
  end

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
