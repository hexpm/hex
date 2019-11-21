defmodule Hex.HTTP.Certs do
  @moduledoc false

  crt_file = Path.join(__DIR__, "ca-bundle.crt")
  crt = File.read!(crt_file)

  pems = :public_key.pem_decode(crt)
  ders = Enum.map(pems, fn {:Certificate, der, _} -> der end)

  @der_encoded ders
  @external_resource crt_file

  def cacerts do
    @der_encoded
  end

  def decode_runtime(path) do
    crt = File.read!(path)
    pems = :public_key.pem_decode(crt)
    Enum.map(pems, fn {:Certificate, der, _} -> der end)
  end
end
