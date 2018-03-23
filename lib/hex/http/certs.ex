defmodule Hex.HTTP.Certs do
  def cacerts do
    case Hex.State.fetch(:ders) do
      {:ok, value} -> value
      :error -> decode_and_store_ca_certs()
    end
  end

  defp decode_and_store_ca_certs() do
    crt_file = Hex.State.fetch!(:cafile)
    crt = File.read!(crt_file)

    pems = :public_key.pem_decode(crt)
    ders = Enum.map(pems, fn {:Certificate, der, _} -> der end)

    Hex.State.put(:ders, ders)
    ders
  end
end
