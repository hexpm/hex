defmodule Hex.Repo do
  @moduledoc false

  @request_timeout 60_000

  def request(url, etag) do
    opts = [body_format: :binary]
    headers = [{'user-agent', Hex.API.user_agent}]
    headers = if etag, do: [{'if-none-match', '"' ++ etag ++ '"'}|headers], else: headers
    http_opts = [relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    url = String.to_char_list(url)
    profile = Hex.State.fetch!(:httpc_profile)

    case Hex.API.request_with_redirect(:get, {url, headers}, http_opts, opts, profile, 3) do
      {:ok, {{_version, 200, _reason}, headers, body}} ->
        headers = Enum.into(headers, %{})
        etag = headers['etag']
        etag = if etag, do: List.to_string(etag)
        {:ok, body, etag}
      {:ok, {{_version, 304, _reason}, _headers, _body}} ->
        {:ok, :cached}
      {:ok, {{_version, code, _reason}, _headers, _body}} ->
        {:error, "Request failed (#{code})"}
      {:error, reason} ->
        {:error, "Request failed (#{inspect reason})"}
    end
  end
end
