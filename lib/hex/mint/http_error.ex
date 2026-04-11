# Vendored from mint v1.7.1 (f7f0313), do not edit manually

defmodule Hex.Mint.HTTPError do
  _ = """
  An HTTP error.

  This exception struct is used to represent HTTP errors of all sorts and for
  both HTTP/1 and HTTP/2.

  A `Hex.Mint.HTTPError` struct is an exception, so it can be raised as any
  other exception.

  ## Struct

  The `Hex.Mint.HTTPError` struct is opaque, that is, not all of its fields are public.
  The list of public fields is:

    * `:reason` - the error reason. Can be one of:

      * a term of type `t:Hex.Mint.HTTP1.error_reason/0`. See its documentation for
        more information.

      * a term of type `t:Hex.Mint.HTTP2.error_reason/0`. See its documentation for
        more information.

      * `{:proxy, reason}`, which is used when an HTTP error happens when connecting
        to a tunnel proxy. `reason` can be:

        * `:tunnel_timeout` - when the tunnel times out.

        * `{:unexpected_status, status}` - when the proxy returns an unexpected
          status `status`.

        * `{:unexpected_trailing_responses, responses}` - when the proxy returns
          unexpected responses (`responses`).

  ## Message representation

  If you want to convert an error reason to a human-friendly message (for example
  for using in logs), you can use `Exception.message/1`:

      iex> {:error, %Hex.Mint.HTTPError{} = error} = Hex.Mint.HTTP.connect(:http, "bad-response.com", 80)
      iex> Exception.message(error)
      "the response contains two or more Content-Length headers"

  """

  alias Hex.Mint.{HTTP1, HTTP2}

  @type proxy_reason() ::
          {:proxy,
           HTTP1.error_reason()
           | HTTP2.error_reason()
           | :tunnel_timeout
           | {:unexpected_status, non_neg_integer()}
           | {:unexpected_trailing_responses, list()}}

  @typedoc """
  The error reason.
  """
  @typedoc since: "1.7.2"
  @type reason :: HTTP1.error_reason() | HTTP2.error_reason() | proxy_reason() | term()

  @typedoc """
  Type for the `Hex.Mint.HTTPError` exception struct.
  """
  @type t() :: %__MODULE__{reason: reason()}

  defexception [:reason, :module]

  @impl true
  def message(%__MODULE__{reason: reason, module: module}) do
    module.format_error(reason)
  end
end
