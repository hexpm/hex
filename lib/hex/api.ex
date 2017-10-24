defmodule Hex.API do
  alias Hex.HTTP

  @erlang_content 'application/vnd.hex+erlang'
  @tar_content 'application/octet-stream'

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
    HTTP.request(:post, url(repo, path), headers(opts), encode_tar(body, progress), opts)
    |> handle_response()
  end

  defp url(repo, path) do
    api_url = Hex.State.fetch!(:api_url)
    api_url <> repo_path(repo) <> path
  end

  defp repo_path(nil), do: "/"
  defp repo_path("hexpm"), do: "/"
  defp repo_path(org), do: "/repos/#{org}/"

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
    {@erlang_content, Hex.Utils.safe_serialize_erlang(body), & &1}
  end

  defp encode_tar(body, progress) do
    {@tar_content, body, progress}
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
