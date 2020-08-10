defmodule Hex.HTTP.SSL do
  @moduledoc false

  require Record
  alias Hex.HTTP.Certs
  alias Hex.HTTP.VerifyHostname

  # From https://www.ssllabs.com/ssltest/clients.html Android 7
  @default_ciphers [
    'AES128-GCM-SHA256',
    'AES128-SHA',
    'AES256-GCM-SHA384',
    'AES256-SHA',
    'DES-CBC3-SHA',
    'ECDHE-ECDSA-AES128-GCM-SHA256',
    'ECDHE-ECDSA-AES128-SHA',
    'ECDHE-ECDSA-AES256-GCM-SHA384',
    'ECDHE-ECDSA-AES256-SHA',
    'ECDHE-ECDSA-CHACHA20-POLY1305-SHA256',
    'ECDHE-RSA-AES128-GCM-SHA256',
    'ECDHE-RSA-AES128-SHA',
    'ECDHE-RSA-AES256-GCM-SHA384',
    'ECDHE-RSA-AES256-SHA',
    'ECDHE-RSA-CHACHA20-POLY1305-SHA256'
  ]

  @default_versions [:"tlsv1.2", :"tlsv1.1", :tlsv1]

  @secure_ssl_version {5, 3, 7}

  Record.defrecordp(
    :certificate,
    :OTPCertificate,
    Record.extract(:OTPCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  Record.defrecordp(
    :tbs_certificate,
    :OTPTBSCertificate,
    Record.extract(:OTPTBSCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")
  )

  def secure_ssl? do
    check? = not Hex.State.fetch!(:unsafe_https)

    if check? and Hex.State.fetch!(:ssl_version) <= @secure_ssl_version do
      Mix.raise(
        "Insecure HTTPS request (peer verification disabled), " <>
          "please update to OTP 17.4 or later, or disable by setting " <>
          "the environment variable HEX_UNSAFE_HTTPS=1"
      )
    end

    check?
  end

  def get_ca_certs do
    case Hex.State.fetch!(:cacerts_path) do
      nil -> Certs.cacerts()
      path -> Certs.decode_runtime(path)
    end
  end

  def ssl_opts(url) do
    hostname = Hex.string_to_charlist(URI.parse(url).host)
    ciphers = filter_ciphers(@default_ciphers)

    if secure_ssl?() do
      partial_chain = &partial_chain(Certs.cacerts(), &1)

      [
        verify: :verify_peer,
        depth: 4,
        partial_chain: partial_chain,
        cacerts: get_ca_certs(),
        server_name_indication: hostname,
        secure_renegotiate: true,
        reuse_sessions: true,
        versions: @default_versions,
        ciphers: ciphers
      ]
      |> customize_hostname_check(hostname)
    else
      [
        verify: :verify_none,
        server_name_indication: hostname,
        secure_renegotiate: true,
        reuse_sessions: true,
        versions: @default_versions,
        ciphers: ciphers
      ]
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

  defp filter_ciphers(allowed) do
    available = Hex.Set.new(:ssl.cipher_suites(:openssl))
    Enum.filter(allowed, &(&1 in available))
  end

  defp customize_hostname_check(opts, hostname) do
    if ssl_major_version() >= 9 do
      # From OTP 20.0 use built-in support for custom hostname checks
      Keyword.put(opts, :customize_hostname_check, match_fun: &VerifyHostname.match_fun/2)
    else
      # Before OTP 20.0 use mint_shims for hostname check, from a custom verify_fun
      Keyword.put(opts, :verify_fun, {&VerifyHostname.verify_fun/3, check_hostname: hostname})
    end
  end

  defp ssl_major_version do
    # Elixir 1.0.5 - 1.1.1 have no Application.spec/2
    case :application.get_key(:ssl, :vsn) do
      {:ok, value} -> value
      :undefined -> nil
    end
    |> :string.to_integer()
    |> elem(0)
  end
end
