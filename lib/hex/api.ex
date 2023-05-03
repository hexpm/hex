defmodule Hex.API do
  @moduledoc false

  alias Hex.HTTP

  @erlang_content "application/vnd.hex+erlang"
  @tar_content "application/octet-stream"
  @tar_chunk_size 10_000
  @tar_timeout 60_000

  def request(method, repo, path, opts \\ []) do
    HTTP.request(method, url(repo, path), headers(opts), nil)
    |> handle_response()
  end

  def erlang_post_request(repo, path, body, opts \\ []) do
    HTTP.request(:post, url(repo, path), headers(opts), encode_erlang(body))
    |> handle_response()
  end

  def erlang_put_request(repo, path, body, opts \\ []) do
    HTTP.request(:put, url(repo, path), headers(opts), encode_erlang(body))
    |> handle_response()
  end

  def tar_post_request(repo, path, body, opts \\ []) do
    progress = Keyword.fetch!(opts, :progress)

    headers =
      %{"content-length" => Integer.to_string(byte_size(body))}
      |> Map.merge(headers(opts))

    opts = [
      timeout:
        Hex.State.fetch!(:http_timeout, fn val -> if is_integer(val), do: val * 1000 end) ||
          @tar_timeout
    ]

    HTTP.request(:post, url(repo, path), headers, encode_tar(body, progress), opts)
    |> handle_response()
  end

  defp url(repo, path) do
    api_url = Hex.State.fetch!(:api_url)
    api_url <> repo_path(repo) <> path
  end

  defp repo_path(nil), do: "/"
  defp repo_path(org), do: "/repos/#{org}/"

  defp headers(opts) do
    %{"accept" => @erlang_content}
    |> Map.merge(auth(opts))
  end

  defp auth(opts) do
    cond do
      opts[:key] ->
        %{"authorization" => opts[:key]}

      opts[:user] && opts[:pass] ->
        base64 = :base64.encode(opts[:user] <> ":" <> opts[:pass])
        %{"authorization" => "Basic " <> base64}

      true ->
        %{}
    end
  end

  defp encode_erlang(body) do
    {@erlang_content, Hex.Utils.safe_serialize_erlang(body)}
  end

  defp encode_tar(body, progress) do
    body = fn
      size when size < byte_size(body) ->
        new_size = min(size + @tar_chunk_size, byte_size(body))
        chunk = new_size - size
        progress.(new_size)
        {:ok, :binary.part(body, size, chunk), new_size}

      _size ->
        :eof
    end

    {@tar_content, {body, 0}}
  end

  defp handle_response({:ok, code, headers, body}) do
    {:ok, {code, decode_body(body, headers), headers}}
  end

  defp handle_response({:error, term}) do
    {:error, term}
  end

  defp decode_body(body, headers) do
    content_type = headers["content-type"] || ""

    if String.contains?(content_type, @erlang_content) do
      Hex.Utils.safe_deserialize_erlang(body)
    else
      body
    end
  end

  def check_write_api() do
    case Application.load(:ssl) do
      :ok ->
        if :application.get_key(:ssl, :vsn) == {:ok, ~c"10.2"} do
          Mix.raise("""
          You are using an OTP release with the application ssl-10.2 which has a vulnerability \
          making it susceptible to man-in-the-middle attacks. API operations with write
          capabilities are disabled until you upgrade to newer version, ssl-10.2.1+ or OTP-23.2.2+.
          """)
        end

      {:error, _} ->
        :ok
    end
  end
end
