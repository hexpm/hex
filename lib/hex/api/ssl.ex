defmodule Hex.API.SSL do
  require Record
  alias Hex.API.VerifyHostname

  # From https://www.ssllabs.com/ssltest/clients.html Android 7
  @default_ciphers [
    'ECDHE-ECDSA-CHACHA20-POLY1305-SHA256',
    'ECDHE-RSA-CHACHA20-POLY1305-SHA256',
    'ECDHE-ECDSA-AES128-GCM-SHA256',
    'ECDHE-RSA-AES128-GCM-SHA256',
    'ECDHE-ECDSA-AES256-GCM-SHA384',
    'ECDHE-RSA-AES256-GCM-SHA384',
    'ECDHE-ECDSA-AES128-SHA',
    'ECDHE-RSA-AES128-SHA',
    'ECDHE-ECDSA-AES256-SHA',
    'ECDHE-RSA-AES256-SHA',
    'AES128-GCM-SHA256',
    'AES256-GCM-SHA384',
    'AES128-SHA',
    'AES256-SHA',
    'DES-CBC3-SHA'
  ]

  @default_versions [:"tlsv1.2", :"tlsv1.1", :tlsv1]

  @secure_ssl_version {5, 3, 7}

  Record.defrecordp :certificate, :OTPCertificate,
    Record.extract(:OTPCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  Record.defrecordp :tbs_certificate, :OTPTBSCertificate,
    Record.extract(:OTPTBSCertificate, from_lib: "public_key/include/OTP-PUB-KEY.hrl")

  def secure_ssl? do
    check? = Hex.State.fetch!(:check_cert?)
    if check? and Hex.State.fetch!(:ssl_version) <= @secure_ssl_version do
      Mix.raise "Insecure HTTPS request (peer verification disabled), " <>
                "please update to OTP 17.4 or later, or disable by setting " <>
                "the environment variable HEX_UNSAFE_HTTPS=1"
    end
    check?
  end

  def ssl_opts(url) do
    url = List.to_string(url)
    hostname = Hex.string_to_charlist(URI.parse(url).host)
    ciphers = filter_ciphers(@default_ciphers)

    if secure_ssl?() do
      verify_fun = {&VerifyHostname.verify_fun/3, check_hostname: hostname}
      partial_chain = &partial_chain(Hex.API.Certs.cacerts, &1)

      [verify: :verify_peer,
       depth: 4,
       partial_chain: partial_chain,
       cacerts: Hex.API.Certs.cacerts(),
       verify_fun: verify_fun,
       server_name_indication: hostname,
       secure_renegotiate: true,
       reuse_sessions: true,
       honor_cipher_order: true,
       versions: @default_versions,
       ciphers: ciphers]
    else
      [verify: :verify_none,
       server_name_indication: hostname,
       secure_renegotiate: true,
       reuse_sessions: true,
       honor_cipher_order: true,
       versions: @default_versions,
       ciphers: ciphers]
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
end
