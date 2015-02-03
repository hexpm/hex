defmodule Hex.API do
  alias Hex.API.Util
  alias Hex.API.VerifyHostname

  @secure_ssl_version {5, 3, 6}

  require Record

  Record.defrecordp :certificate, :OTPCertificate,
    Record.extract(:Certificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :tbs_certificate, :OTPTBSCertificate,
    Record.extract(:OTPTBSCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :subject_public_key_info, :OTPSubjectPublicKeyInfo,
    Record.extract(:OTPSubjectPublicKeyInfo, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :algorithm_identifier, :AlgorithmIdentifier,
    Record.extract(:AlgorithmIdentifier, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  def request(method, url, headers, body \\ nil) when body == nil or is_map(body) do
    default_headers = %{
      'accept' => 'application/vnd.hex.beta+elixir',
      'accept-encoding' => 'gzip',
      'user-agent' => user_agent()}
    headers = Dict.merge(default_headers, headers)

    http_opts = [ssl: ssl_opts(url), relaxed: true]
    opts = [body_format: :binary]
    url = String.to_char_list(url)

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

  def ssl_opts(url) do
    if ssl_version() >= @secure_ssl_version do
      hostname = String.to_char_list(URI.parse(url).host)
      verify_fun = {&VerifyHostname.verify_fun/3, check_hostname: hostname}

      [verify: :verify_peer, depth: 2, partial_chain: &partial_chain/1,
       cacerts: Hex.API.Certs.cacerts(), verify_fun: verify_fun]
    else
      [verify: :verify_none]
    end
  end

  defp partial_chain(certs) do
    certs = Enum.map(certs, &{&1, :public_key.pkix_decode_cert(&1, :otp)})
    cacerts = Hex.API.Certs.cacerts()
    cacerts = Enum.map(cacerts, &:public_key.pkix_decode_cert(&1, :otp))

    trusted =
      Enum.find_value(certs, fn {der, cert} ->
        trust? = Enum.any?(cacerts, fn cacert ->
          if :public_key.pkix_is_issuer(cert, cacert) do
            key = extract_key(cacert)
            :public_key.pkix_verify(der, key)
          end
        end)

        if trust?, do: der
      end)

    if trusted do
      {:trusted_ca, trusted}
    else
      :unknown_ca
    end
  end

  defp extract_key(cert) do
    cert
    |> certificate(:tbsCertificate)
    |> tbs_certificate(:subjectPublicKeyInfo)
    |> subject_public_key_info(:subjectPublicKey)
  end

  @chunk 10_000

  def request_tar(method, url, headers, body, progress) do
    default_headers = %{
      'accept' => 'application/vnd.hex.beta+elixir',
      'user-agent' => user_agent(),
      'content-length' => to_char_list(byte_size(body))}
    headers = Dict.merge(default_headers, headers)
    http_opts = [ssl: ssl_opts(url), relaxed: true]
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
    Hex.cdn <> "/" <> path
  end

  def url(path) do
    Hex.url <> "/" <> path
  end

  def api_url(path) do
    Hex.url <> "/api/" <> path
  end

  def auth(key: secret) do
    %{'authorization' => String.to_char_list(secret)}
  end

  def auth(info) do
    base64 = :base64.encode_to_string(info[:user] <> ":" <> info[:pass])
    %{'authorization' => 'Basic ' ++ base64}
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

  defp warn_ssl_version(_version) do
    # Don't emit the warning just yet.
    # Wait ~2 weeks (2014-09-17)

    # if version < @secure_ssl_version do
    #   Mix.shell.error("Insecure HTTPS request (peer verification disabled), " <>
    #                   "please update to OTP 17.3 or later")
    # end
  end

  defp parse_ssl_version(version) do
    version
    |> List.to_string
    |> String.split(".")
    |> Enum.take(3)
    |> Enum.map(&to_integer/1)
    |> List.to_tuple
  end

  defp to_integer(string) do
    {int, _} = Integer.parse(string)
    int
  end
end
