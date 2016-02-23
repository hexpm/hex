defmodule Hex.API do
  alias Hex.API.Utils
  alias Hex.API.VerifyHostname

  @request_timeout 60_000
  @secure_ssl_version {5, 3, 6}

  require Record

  Record.defrecordp :certificate, :OTPCertificate,
    Record.extract(:OTPCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :tbs_certificate, :OTPTBSCertificate,
    Record.extract(:OTPTBSCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  def request(method, url, headers, body \\ nil) when body == nil or is_map(body) do
    default_headers = %{
      'accept' => 'application/vnd.hex+erlang',
      'accept-encoding' => 'gzip',
      'user-agent' => user_agent()}
    headers = Dict.merge(default_headers, headers)

    http_opts = [ssl: ssl_opts(url), relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    opts = [body_format: :binary]
    url = String.to_char_list(url)

    request =
      cond do
        body ->
          body = Hex.Utils.safe_serialize_erlang(body)
          {url, Map.to_list(headers), 'application/vnd.hex+erlang', body}
        method in [:put, :post] ->
          body = :erlang.term_to_binary(%{})
          {url, Map.to_list(headers), 'application/vnd.hex+erlang', body}
        true ->
          {url, Map.to_list(headers)}
      end

    case :httpc.request(method, request, http_opts, opts, :hex) do
      {:ok, response} ->
        handle_response(response)
      {:error, reason} ->
        {:http_error, reason}
    end
  end

  def secure_ssl? do
    ssl_version() >= @secure_ssl_version and Hex.State.fetch!(:cert_check?)
  end

  def ssl_opts(url) do
    if secure_ssl?() do
      hostname      = String.to_char_list(URI.parse(url).host)
      verify_fun    = {&VerifyHostname.verify_fun/3, check_hostname: hostname}
      partial_chain = &partial_chain(Hex.API.Certs.cacerts, &1)

      [verify: :verify_peer, depth: 2, partial_chain: partial_chain,
       cacerts: Hex.API.Certs.cacerts(), verify_fun: verify_fun,
       server_name_indication: hostname]
    else
      [verify: :verify_none]
    end
  end

  def partial_chain(cacerts, certs) do
    certs = Enum.map(certs, &{&1, :public_key.pkix_decode_cert(&1, :otp)})
    cacerts = Enum.map(cacerts, &:public_key.pkix_decode_cert(&1, :otp))

    trusted =
      Enum.find_value(certs, fn {der, cert} ->
        trusted? =
          Enum.find(cacerts, fn cacert ->
            extract_public_key_info(cacert) == extract_public_key_info(cert)
          end)

        if trusted?, do: der
      end)

    if trusted do
      {:trusted_ca, trusted}
    else
      :unknown_ca
    end
  end

  defp extract_public_key_info(cert) do
    cert
    |> certificate(:tbsCertificate)
    |> tbs_certificate(:subjectPublicKeyInfo)
  end

  @chunk 10_000

  def request_tar(method, url, headers, body, progress) do
    default_headers = %{
      'accept' => 'application/vnd.hex+erlang',
      'user-agent' => user_agent(),
      'content-length' => to_char_list(byte_size(body))}
    headers = Dict.merge(default_headers, headers)
    http_opts = [ssl: ssl_opts(url), relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    opts = [body_format: :binary]
    url = String.to_char_list(url)

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
    Utils.handle_hex_message(headers['x-hex-message'])

    body = body |> unzip(headers) |> decode(headers)

    {code, body}
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
    if String.contains?(content_type, "application/vnd.hex+erlang") do
      Hex.Utils.safe_deserialize_erlang(body)
    else
      body
    end
  end

  def user_agent do
    'Hex/#{Hex.version} (Elixir/#{System.version}) (OTP/#{Hex.Utils.otp_version})'
  end

  def cdn_url(path) do
    Hex.State.fetch!(:cdn) <> "/" <> path
  end

  def api_url(path) do
    Hex.State.fetch!(:api) <> "/" <> path
  end

  def auth(opts) do
    if key = opts[:key] do
      %{'authorization' => String.to_char_list(key)}
    else
      base64 = :base64.encode_to_string(opts[:user] <> ":" <> opts[:pass])
      %{'authorization' => 'Basic ' ++ base64}
    end
  end

  defp ssl_version do
    case Application.fetch_env(:hex, :ssl_version) do
      {:ok, version} ->
        version
      :error ->
        {:ok, version} = :application.get_key(:ssl, :vsn)
        version = parse_ssl_version(version)

        warn_ssl_version(version)
        Application.put_env(:hex, :ssl_version, version)
        version
    end
  end

  defp warn_ssl_version(version) do
    if version < @secure_ssl_version do
      Hex.Shell.warn "Insecure HTTPS request (peer verification disabled), " <>
                     "please update to OTP 17.4 or later"
    end
  end

  defp parse_ssl_version(version) do
    version
    |> List.to_string
    |> String.split(".")
    |> Enum.take(3)
    |> Enum.map(&to_integer/1)
    |> version_pad
    |> List.to_tuple
  end

  defp version_pad([major]),
    do: [major, 0, 0]
  defp version_pad([major, minor]),
    do: [major, minor, 0]
  defp version_pad([major, minor, patch]),
    do: [major, minor, patch]
  defp version_pad([major, minor, patch | _]),
    do: [major, minor, patch]

  defp to_integer(string) do
    {int, _} = Integer.parse(string)
    int
  end
end
