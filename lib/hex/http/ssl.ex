defmodule Hex.HTTP.SSL do
  @moduledoc false

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

  def get_ca_certs do
    case Hex.State.fetch!(:cacerts_path) do
      nil -> Certs.cacerts()
      path -> Certs.decode_runtime(path)
    end
  end

  def ssl_opts(url) do
    hostname = String.to_charlist(URI.parse(url).host)
    ciphers = filter_ciphers(@default_ciphers)

    if Hex.State.fetch!(:unsafe_https) do
      [
        verify: :verify_none,
        server_name_indication: hostname,
        secure_renegotiate: true,
        reuse_sessions: true,
        versions: @default_versions,
        ciphers: ciphers
      ]
    else
      [
        verify: :verify_peer,
        depth: 4,
        cacerts: get_ca_certs(),
        server_name_indication: hostname,
        secure_renegotiate: true,
        reuse_sessions: true,
        versions: @default_versions,
        ciphers: ciphers
      ]
      |> customize_hostname_check()
    end
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
