defmodule Hex.TrustedPublisher do
  @moduledoc false

  @doc """
  Whether publishing should authenticate as a trusted publisher.

  Only a GitHub Actions job allowed to request OIDC tokens qualifies, and any
  credential the user configured takes precedence.
  """
  def available? do
    github_oidc_request() != nil and not configured_credentials?()
  end

  @doc """
  Exchanges the job's OIDC token for API auth scoped to one package.
  """
  def auth!(repository, package) do
    {url, request_token} = github_oidc_request()
    scope = "package:#{repository}/#{package}"

    audience = audience!()
    oidc_token = github_oidc_token!(url, request_token, audience)
    [key: exchange!(oidc_token, scope), oauth: true]
  end

  defp configured_credentials? do
    Hex.State.get(:api_key) != nil or Hex.OAuth.has_tokens?() or hexpm_api_key?()
  end

  defp hexpm_api_key? do
    match?({:ok, %{api_key: key}} when is_binary(key), Hex.Repo.fetch_repo("hexpm"))
  end

  defp github_oidc_request do
    url = System.get_env("ACTIONS_ID_TOKEN_REQUEST_URL")
    token = System.get_env("ACTIONS_ID_TOKEN_REQUEST_TOKEN")

    if present?(url) and present?(token) do
      {url, token}
    end
  end

  defp present?(value), do: is_binary(value) and value != ""

  defp audience! do
    case Hex.API.OIDC.audience() do
      {:ok, {200, _headers, %{"audience" => audience}}} when is_binary(audience) ->
        audience

      other ->
        Mix.raise(
          "Trusted publishing failed, could not fetch the OIDC audience from Hex: " <>
            describe_error(other)
        )
    end
  end

  defp github_oidc_token!(url, request_token, audience) do
    headers = %{
      "authorization" => "Bearer #{request_token}",
      "accept" => "application/json"
    }

    case Hex.HTTP.request(:get, put_audience(url, audience), headers, nil) do
      {:ok, {200, _headers, body}} ->
        case decode_json(body) do
          %{"value" => token} when is_binary(token) and token != "" ->
            token

          _other ->
            Mix.raise("Trusted publishing failed, GitHub Actions answered without an OIDC token")
        end

      {:ok, {status, _headers, _body}} ->
        Mix.raise(
          "Trusted publishing failed, GitHub Actions refused to issue an OIDC token " <>
            "(HTTP #{status}). Make sure the workflow has the `id-token: write` permission"
        )

      {:error, reason} ->
        Mix.raise(
          "Trusted publishing failed, could not request an OIDC token from GitHub Actions: " <>
            inspect(reason)
        )
    end
  end

  defp put_audience(url, audience) do
    uri = URI.parse(url)
    param = URI.encode_query(%{"audience" => audience})
    query = if uri.query in [nil, ""], do: param, else: uri.query <> "&" <> param
    URI.to_string(%{uri | query: query})
  end

  defp exchange!(oidc_token, scope) do
    case Hex.API.OAuth.jwt_bearer_token(oidc_token, scope) do
      {:ok, {200, _headers, %{"access_token" => token}}} when is_binary(token) ->
        token

      other ->
        Mix.raise(
          "Trusted publishing failed, Hex did not grant a token for #{scope}: " <>
            describe_error(other)
        )
    end
  end

  defp describe_error({:ok, {status, _headers, %{"error" => error} = body}}) do
    case body["error_description"] do
      nil -> "#{error} (HTTP #{status})"
      description -> "#{error}: #{description} (HTTP #{status})"
    end
  end

  defp describe_error({:ok, {status, _headers, %{"message" => message}}}) do
    "#{message} (HTTP #{status})"
  end

  defp describe_error({:ok, {status, _headers, _body}}), do: "HTTP #{status}"
  defp describe_error({:error, reason}), do: inspect(reason)

  defp decode_json(body) do
    cond do
      Code.ensure_loaded?(:json) ->
        safe_decode(fn -> :json.decode(body) end)

      Code.ensure_loaded?(JSON) ->
        safe_decode(fn -> apply(JSON, :decode!, [body]) end)

      true ->
        Mix.raise("Trusted publishing requires OTP 27 or later, or Elixir 1.18 or later")
    end
  end

  defp safe_decode(fun) do
    fun.()
  rescue
    _error -> nil
  end
end
