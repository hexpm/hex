defmodule Hex.HTTPIntegrationTest do
  # End-to-end HTTPS round-trips against the real hex.pm + repo.hex.pm
  # servers. Exercises the full Mint pipeline: TLS handshake, certificate
  # verification, ALPN protocol negotiation (HTTP/2 where the server offers
  # it), the connection pool, and response decoding.
  #
  # These tests require working internet access and that hex.pm +
  # repo.hex.pm are reachable. Tagged `:network` so they can be excluded
  # with `mix test --exclude network` if the environment is offline, but
  # they run by default on plain `mix test`.

  use HexTest.Case, async: false

  @moduletag :network

  @hexpm_host "hex.pm"
  @repo_host "repo.hex.pm"

  test "GET https://#{@hexpm_host}/api/packages/phoenix returns JSON metadata" do
    {:ok, {status, headers, body}} =
      Hex.HTTP.request(:get, "https://#{@hexpm_host}/api/packages/phoenix", %{}, nil)

    assert status == 200
    assert headers["content-type"] =~ "application/"
    assert byte_size(body) > 0
    assert body =~ "phoenix"
  end

  test "GET https://#{@repo_host}/names returns the signed names registry" do
    {:ok, {status, _headers, body}} =
      Hex.HTTP.request(:get, "https://#{@repo_host}/names", %{}, nil)

    assert status == 200
    # The names registry is a signed gzipped protobuf; we only assert that
    # the pipeline delivered a non-trivial payload.
    assert byte_size(body) > 100
  end

  test "GET https://#{@repo_host}/packages/phoenix returns the versions record" do
    {:ok, {status, _headers, body}} =
      Hex.HTTP.request(:get, "https://#{@repo_host}/packages/phoenix", %{}, nil)

    assert status == 200
    assert byte_size(body) > 0
  end

  test "pool negotiates a real ALPN protocol for #{@hexpm_host}" do
    {:ok, _} =
      Hex.HTTP.request(:get, "https://#{@hexpm_host}/api/packages/phoenix", %{}, nil)

    host_pid = lookup_host(@hexpm_host)
    %{protocol: protocol, conns: conns} = :sys.get_state(host_pid)

    assert protocol in [:http1, :http2]
    assert map_size(conns) >= 1
  end

  test "pool negotiates a real ALPN protocol for #{@repo_host}" do
    {:ok, _} = Hex.HTTP.request(:get, "https://#{@repo_host}/names", %{}, nil)

    host_pid = lookup_host(@repo_host)
    %{protocol: protocol, conns: conns} = :sys.get_state(host_pid)

    assert protocol in [:http1, :http2]
    assert map_size(conns) >= 1
  end

  test "consecutive requests to the same host reuse the same pool" do
    {:ok, _} = Hex.HTTP.request(:get, "https://#{@repo_host}/names", %{}, nil)
    pid1 = lookup_host(@repo_host)

    {:ok, _} = Hex.HTTP.request(:get, "https://#{@repo_host}/versions", %{}, nil)
    pid2 = lookup_host(@repo_host)

    assert pid1 == pid2
  end

  defp lookup_host(host) do
    case Registry.lookup(Hex.HTTP.Pool.__registry__(), {:https, host, 443, :inet}) do
      [{pid, _}] -> pid
      [] -> flunk("no Hex.HTTP.Pool.Host registered for #{host}")
    end
  end
end
