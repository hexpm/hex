defmodule Hex.API do
  alias Hex.HTTP

  @erlang_content 'application/vnd.hex+erlang'
  @tar_content 'application/octet-stream'
  @tar_chunk_size 10_000

  def request(method, path, opts \\ []) do
    HTTP.request(method, url(path), headers(opts), nil)
    |> handle_response()
  end

  def erlang_post_request(path, body, opts \\ []) do
    HTTP.request(:post, url(path), headers(opts), encode_erlang(body))
    |> handle_response()
  end

  def erlang_put_request(path, body, opts \\ []) do
    HTTP.request(:put, url(path), headers(opts), encode_erlang(body))
    |> handle_response()
  end

  def tar_post_request(path, body, opts \\ []) do
    progress = Keyword.fetch!(opts, :progress)
    headers =
      %{'content-length' => Hex.to_charlist(byte_size(body))}
      |> Map.merge(headers(opts))
    HTTP.request(:post, url(path), headers, encode_tar(body, progress))
    |> handle_response()
  end

  defp url(path) do
    Hex.State.fetch!(:api) <> "/" <> path
  end

  defp headers(opts) do
    %{'accept' => @erlang_content}
    |> Map.merge(auth(opts))
  end

  defp auth(opts) do
    cond do
      opts[:key] ->
        %{'authorization' => Hex.string_to_charlist(opts[:key])}
      opts[:user] && opts[:pass] ->
        base64 = :base64.encode_to_string(opts[:user] <> ":" <> opts[:pass])
        %{'authorization' => 'Basic ' ++ base64}
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
      {:ok, [:binary.part(body, size, chunk)], new_size}
      _size ->
        :eof
    end
    {@tar_content, {body, 0}}
  end

  defp handle_response({:ok, {code, body, headers}}) do
    {:ok, {code, decode_body(body, headers), headers}}
  end
  defp handle_response({:error, term}) do
    {:error, term}
  end

  defp decode_body(body, headers) do
    content_type = List.to_string(headers['content-type'] || '')
    erlang_vendor = List.to_string(@erlang_content)

    if String.contains?(content_type, erlang_vendor) do
      Hex.Utils.safe_deserialize_erlang(body)
    else
      body
    end
  end
end
