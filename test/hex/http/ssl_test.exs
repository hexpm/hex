defmodule Hex.HTTP.SSLTest do
  use HexTest.Case

  alias Hex.HTTP.SSL

  setup do
    on_exit(fn -> Hex.State.put(:unsafe_https, false) end)
  end

  test "ssl_opts verifies the peer by default" do
    opts = SSL.ssl_opts("https://repo.hex.pm")

    assert opts[:verify] == :verify_peer
    assert opts[:server_name_indication] == ~c"repo.hex.pm"
    assert is_list(opts[:cacerts])
    assert is_function(opts[:partial_chain], 1)
    assert opts[:customize_hostname_check]
  end

  test "ssl_opts skips peer verification with unsafe_https" do
    Hex.State.put(:unsafe_https, true)
    opts = SSL.ssl_opts("https://repo.hex.pm")

    assert opts[:verify] == :verify_none
    assert opts[:server_name_indication] == ~c"repo.hex.pm"
    refute Keyword.has_key?(opts, :cacerts)
    refute Keyword.has_key?(opts, :partial_chain)
    refute Keyword.has_key?(opts, :customize_hostname_check)
  end
end
