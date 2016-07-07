defmodule Hex.Crypto.ContentEncryptor do

  @type t :: %__MODULE__{
    module: module,
    params: any
  }

  defstruct [
    module: nil,
    params: nil
  ]

  @callback init(options :: Keyword.t)                                            :: {:ok, any} | {:error, String.t}
  @callback encrypt({binary, binary}, params :: any)                              :: {binary, binary}
  @callback decrypt({binary, binary, binary}, params :: any)                      :: {:ok, binary} | :error
  @callback encode(params :: any)                                                 :: {String.t, binary}
  @callback decode(algorithm :: String.t, params :: binary, options :: Keyword.t) :: {:ok, any} | :error | {:error, String.t}

  alias Hex.Crypto

  def init(module, options) do
    case module.init(options) do
      {:ok, params} ->
        {:ok, %__MODULE__{module: module, params: params}}
      error ->
        error
    end
  end

  def encrypt(%__MODULE__{module: module, params: params}, {aad, plain_text}) do
    module.encrypt({aad, plain_text}, params)
  end

  def decrypt(%__MODULE__{module: module, params: params}, {aad, cipher_text, cipher_tag}) do
    module.decrypt({aad, cipher_text, cipher_tag}, params)
  end

  def encode(%__MODULE__{module: module, params: params}) do
    {algorithm, params} = module.encode(params)
    algorithm
    |> Crypto.base64url_encode()
    |> Kernel.<>(".")
    |> Kernel.<>(Crypto.base64url_encode(params))
  end

  def decode(params, options \\ []) do
    case Crypto.base64url_decode(params) do
      {:ok, params} ->
        case String.split(params, ".", parts: 2) do
          [algorithm, params] ->
            case Crypto.base64url_decode(algorithm) do
              {:ok, algorithm} ->
                case Crypto.base64url_decode(params) do
                  {:ok, params} ->
                    algorithm
                    |> case do
                      "A128CBC-HS256" -> Hex.Crypto.AES_CBC_HMAC_SHA2
                      "A192CBC-HS384" -> Hex.Crypto.AES_CBC_HMAC_SHA2
                      "A256CBC-HS512" -> Hex.Crypto.AES_CBC_HMAC_SHA2
                      "A128GCM" -> Hex.Crypto.AES_GCM
                      "A192GCM" -> Hex.Crypto.AES_GCM
                      "A256GCM" -> Hex.Crypto.AES_GCM
                      _ -> :error
                    end
                    |> case do
                      :error -> :error
                      module ->
                        case module.decode(algorithm, params, options) do
                          {:ok, params} ->
                            {:ok, %__MODULE__{module: module, params: params}}
                          decode_error ->
                            decode_error
                        end
                    end
                  _ ->
                    :error
                end
              _ -> :error
            end
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

end