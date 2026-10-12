defmodule Hex.HTTP.SSLTest do
  use HexTest.Case

  require Record
  alias Hex.HTTP.SSL

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

  @secp256r1 {1, 2, 840, 10045, 3, 1, 7}

  @ca [
    {:Extension, {2, 5, 29, 19}, true, {:BasicConstraints, true, :asn1_NOVALUE}},
    {:Extension, {2, 5, 29, 15}, true, [:keyCertSign]}
  ]

  setup do
    on_exit(fn -> Hex.State.put(:cacerts_path, nil) end)
  end

  test "ssl_opts verifies the peer by default" do
    opts = SSL.ssl_opts("https://repo.hex.pm")

    assert opts[:verify] == :verify_peer
    assert opts[:server_name_indication] == ~c"repo.hex.pm"
    assert is_list(opts[:cacerts])
    assert opts[:customize_hostname_check]
  end

  test "ssl_opts trusts the bundled certificates by default" do
    opts = SSL.ssl_opts("https://repo.hex.pm")

    assert opts[:cacerts] == Hex.HTTP.Certs.cacerts()
  end

  test "ssl_opts trusts only the certificates in cacerts_path" do
    path = fixture_path("certs/ca.cert.pem")
    Hex.State.put(:cacerts_path, path)
    opts = SSL.ssl_opts("https://repo.hex.pm")

    assert opts[:cacerts] == Hex.HTTP.Certs.decode_runtime(path)
    refute Keyword.has_key?(opts, :partial_chain)
  end

  test "rejects a server chain to a CA that isn't in cacerts_path" do
    configured = key_pair("Configured CA")
    other = key_pair("Other CA")
    server = key_pair("Server")
    other_root = issue(other, other, @ca)
    port = start_server([issue(server, other, [dns_name("localhost")]), other_root], server)

    put_cacerts([issue(configured, configured, @ca)])
    assert connect(port, "https://localhost") == {:error, :unknown_ca}

    put_cacerts([other_root])
    assert connect(port, "https://localhost") == :ok
  end

  test "accepts a server chain to a root in cacerts_path" do
    root = key_pair("Root")
    old_root = key_pair("Old Root")
    intermediate = key_pair("Intermediate")
    server = key_pair("Server")
    root_cert = issue(root, root, @ca)
    intermediate_cert = issue(intermediate, root, @ca)
    leaf = issue(server, intermediate, [dns_name("localhost")])
    put_cacerts([root_cert])

    chains = [
      [leaf, intermediate_cert],
      [leaf, intermediate_cert, root_cert],
      [leaf, intermediate_cert, issue(root, root, @ca)],
      [leaf, intermediate_cert, issue(root, old_root, @ca)],
      [leaf, intermediate_cert, issue(root, old_root, @ca), issue(old_root, old_root, @ca)]
    ]

    for chain <- chains do
      port = start_server(chain, server)
      assert connect(port, "https://localhost") == :ok
    end
  end

  test "applies the name constraints of an intermediate in cacerts_path" do
    root = key_pair("Root")
    intermediate = key_pair("Intermediate")
    server = key_pair("Server")
    root_cert = issue(root, root, @ca)
    reissued_root = issue(root, root, @ca)
    intermediate_cert = issue(intermediate, root, [permitted_dns_name("example.org") | @ca])
    permitted = issue(server, intermediate, [dns_name("hex.example.org")])
    excluded = issue(server, intermediate, [dns_name("localhost")])

    put_cacerts([root_cert, intermediate_cert])
    port = start_server([permitted, intermediate_cert, reissued_root], server)
    assert connect(port, "https://hex.example.org") == :ok
    port = start_server([excluded, intermediate_cert, reissued_root], server)
    assert connect(port, "https://localhost") == {:error, :bad_certificate}

    put_cacerts([intermediate_cert])
    port = start_server([excluded, intermediate_cert], server)
    assert connect(port, "https://localhost") == {:error, :unknown_ca}
  end

  test "doesn't trust a leaf certificate in cacerts_path as a CA" do
    root = key_pair("Root")
    server = key_pair("Server")
    other_server = key_pair("Other Server")
    leaf = issue(server, root, [dns_name("localhost")])
    put_cacerts([leaf])

    port = start_server([leaf, issue(root, root, @ca)], server)
    assert connect(port, "https://hex.example.org") == {:error, :unknown_ca}

    other_leaf = issue(other_server, server, [dns_name("hex.example.org")])
    port = start_server([other_leaf, leaf], other_server)
    assert connect(port, "https://hex.example.org") == {:error, :unknown_ca}
  end

  defp key_pair(name) do
    %{cert: cert, key: key} =
      :public_key.pkix_test_root_cert(String.to_charlist(name),
        key: {:namedCurve, @secp256r1},
        digest: :sha256
      )

    tbs = cert |> :public_key.pkix_decode_cert(:otp) |> certificate(:tbsCertificate)
    %{key: key, tbs: tbs}
  end

  defp issue(subject, issuer, extensions) do
    subject.tbs
    |> tbs_certificate(
      serialNumber: System.unique_integer([:positive]),
      issuer: tbs_certificate(issuer.tbs, :subject),
      extensions: extensions
    )
    |> :public_key.pkix_sign(issuer.key)
  end

  defp dns_name(host) do
    {:Extension, {2, 5, 29, 17}, false, [{:dNSName, String.to_charlist(host)}]}
  end

  defp permitted_dns_name(domain) do
    subtree = {:GeneralSubtree, {:dNSName, String.to_charlist(domain)}, 0, :asn1_NOVALUE}
    {:Extension, {2, 5, 29, 30}, true, {:NameConstraints, [subtree], :asn1_NOVALUE}}
  end

  defp put_cacerts(certs) do
    path = tmp_path("ssl_test/#{System.unique_integer([:positive])}.pem")
    pem = :public_key.pem_encode(Enum.map(certs, &{:Certificate, &1, :not_encrypted}))
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, pem)
    Hex.State.put(:cacerts_path, path)
  end

  defp start_server(chain, entity) do
    key = {:ECPrivateKey, :public_key.der_encode(:ECPrivateKey, entity.key)}
    {:ok, listen} = :ssl.listen(0, cert: chain, key: key, versions: [:"tlsv1.2"])
    {:ok, {_address, port}} = :ssl.sockname(listen)
    spawn_link(fn -> accept(listen) end)
    port
  end

  defp accept(listen) do
    case :ssl.transport_accept(listen) do
      {:ok, socket} ->
        _ = :ssl.handshake(socket, 5_000)
        _ = :ssl.close(socket)
        accept(listen)

      {:error, _reason} ->
        :ok
    end
  end

  # OTP caches client sessions by host and port, so a later handshake with the
  # same server could resume a session without verifying its chain again
  defp connect(port, url) do
    opts = Keyword.merge(SSL.ssl_opts(url), reuse_sessions: false, active: false)

    case :ssl.connect(~c"127.0.0.1", port, opts, 5_000) do
      {:ok, socket} ->
        :ssl.close(socket)
        :ok

      {:error, {:tls_alert, {alert, _description}}} ->
        {:error, alert}
    end
  end
end
