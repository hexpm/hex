defmodule Hex.Repo do
  @moduledoc false

  @request_timeout 60_000

  def request(url, etag) do
    opts = [body_format: :binary]
    headers = [{'user-agent', Hex.API.user_agent}]
    headers = if etag, do: [{'if-none-match', '"' ++ etag ++ '"'}|headers], else: headers
    http_opts = [ssl: Hex.API.ssl_opts(url), relaxed: true, timeout: @request_timeout] ++ Hex.Utils.proxy_config(url)
    url = String.to_char_list(url)
    profile = Hex.State.fetch!(:httpc_profile)

    case :httpc.request(:get, {url, headers}, http_opts, opts, profile) do
      {:ok, {{_version, 200, _reason}, _headers, body}} ->
        {:ok, body}
      {:ok, {{_version, 304, _reason}, _headers, _body}} ->
        {:ok, :cached}
      {:ok, {{_version, code, _reason}, _headers, _body}} ->
        {:error, "Request failed (#{code})"}
      {:error, reason} ->
        {:error, "Request failed (#{inspect reason})"}
    end
  end
end
