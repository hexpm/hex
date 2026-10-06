defmodule Hex.HTTP.SSLTest do
  use HexTest.Case

  test "offers the cipher suites HTTP/2 allows before the ones it prohibits" do
    suites =
      "https://hex.pm"
      |> Hex.HTTP.SSL.ssl_opts()
      |> Keyword.fetch!(:ciphers)
      |> Enum.map(&:ssl.str_to_suite/1)

    http2_allowed? = fn %{key_exchange: key_exchange, cipher: cipher} ->
      key_exchange in [:ecdhe_ecdsa, :ecdhe_rsa] and
        cipher in [:aes_128_gcm, :aes_256_gcm, :chacha20_poly1305]
    end

    {allowed, prohibited} = Enum.split_while(suites, http2_allowed?)

    assert allowed != []
    assert prohibited != []
    refute Enum.any?(prohibited, http2_allowed?)
  end
end
