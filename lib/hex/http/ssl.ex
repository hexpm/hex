defmodule Hex.HTTP.SSL do
  @moduledoc false

  require Record
  alias Hex.HTTP.Certs
  alias Hex.HTTP.VerifyHostname

  # From https://www.ssllabs.com/ssltest/clients.html Android 7
  @default_ciphers [
    ~c"AES128-GCM-SHA256",
    ~c"AES128-SHA",
    ~c"AES256-GCM-SHA384",
    ~c"AES256-SHA",
    ~c"DES-CBC3-SHA",
    ~c"ECDHE-ECDSA-AES128-GCM-SHA256",
    ~c"ECDHE-ECDSA-AES128-SHA",
    ~c"ECDHE-ECDSA-AES256-GCM-SHA384",
    ~c"ECDHE-ECDSA-AES256-SHA",
    ~c"ECDHE-ECDSA-CHACHA20-POLY1305-SHA256",
    ~c"ECDHE-RSA-AES128-GCM-SHA256",
    ~c"ECDHE-RSA-AES128-SHA",
    ~c"ECDHE-RSA-AES256-GCM-SHA384",
    ~c"ECDHE-RSA-AES256-SHA",
    ~c"ECDHE-RSA-CHACHA20-POLY1305-SHA256"
  ]

  @default_versions [:"tlsv1.2", :"tlsv1.1", :tlsv1]

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

  def get_ca_certs do
    case Hex.State.fetch!(:cacerts_path) do
      nil -> Certs.cacerts()
      path -> Certs.decode_runtime(path)
    end
  end

  def ssl_opts(url) do
    hostname = String.to_charlist(URI.parse(url).host)
    ciphers = filter_ciphers(@default_ciphers)

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
    |> customize_hostname_check()
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
    [ssl_version | _] = :ssl.versions()[:supported]
    available = MapSet.new(:ssl.cipher_suites(:all, ssl_version, :openssl))
    Enum.filter(allowed, &(&1 in available))
  end

  defp customize_hostname_check(opts) do
    Keyword.put(opts, :customize_hostname_check, match_fun: &VerifyHostname.match_fun/2)
  end
end
