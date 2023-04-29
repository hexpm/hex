defmodule Hex.HTTP do
  @moduledoc false

  @behaviour :mix_hex_http

  @request_timeout 15_000
  @request_redirects 3
  @request_retries 2

  @impl :mix_hex_http
  def request(method, url, headers, body, opts \\ []) do
    headers =
      headers
      |> build_headers()
      |> add_basic_auth_via_netrc(url)

    timeout =
      opts[:timeout] ||
        Hex.State.fetch!(:http_timeout, fn val -> if is_integer(val), do: val * 1000 end) ||
        @request_timeout

    http_opts = build_http_opts(url, timeout)
    opts = [body_format: :binary]
    request = build_request(url, headers, body)
    profile = Hex.State.fetch!(:httpc_profile)

    retry(method, request, http_opts, @request_retries, profile, fn request, http_opts ->
      redirect(request, http_opts, @request_redirects, fn request, http_opts ->
        timeout(request, http_opts, timeout, fn request, http_opts ->
          :httpc.request(method, request, http_opts, opts, profile)
          |> handle_response()
        end)
      end)
    end)
  end

  defp fallback(:inet), do: :inet6
  defp fallback(:inet6), do: :inet

  defp build_headers(headers) do
    default_headers = %{"user-agent" => user_agent()}

    Map.merge(default_headers, headers)
  end

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
        {url, headers, String.to_charlist(content_type), body}

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

  defp handle_response({:ok, {{_version, code, _reason}, headers, body}}) do
    headers = Map.new(headers, &decode_header/1)

    handle_hex_message(headers["x-hex-message"])
    {:ok, code, headers, unzip(body, headers)}
  end

  defp handle_response({:error, term}) do
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

  defp user_agent do
    "Hex/#{Hex.version()} (Elixir/#{System.version()}) (OTP/#{Hex.Utils.otp_version()})"
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
end
