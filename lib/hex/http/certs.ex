defmodule Hex.HTTP.Certs do
  crt_file = Path.join(__DIR__, "ca-bundle.crt")
  crt = File.read!(crt_file)

  pems = :public_key.pem_decode(crt)
  ders = Enum.map(pems, fn {:Certificate, der, _} -> der end)

  @der_encoded ders
  @external_resource crt_file

  def cacerts do
    @der_encoded
  end
end
