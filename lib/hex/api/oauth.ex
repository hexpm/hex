defmodule Hex.API.OAuth do
  @moduledoc false

  alias Hex.API.Client

  @client_id "78ea6566-89fd-481e-a1d6-7d9d78eacca8"

  @doc """
  Initiates the OAuth device authorization flow.

  Returns device code, user code, and verification URIs for user authentication.

  ## Examples

      iex> Hex.API.OAuth.device_authorization()
      {:ok, {200, _headers, %{
        "device_code" => "...",
        "user_code" => "ABCD-1234",
        "verification_uri" => "https://hex.pm/oauth/device",
        "verification_uri_complete" => "https://hex.pm/oauth/device?user_code=ABCD-1234",
        "expires_in" => 600,
        "interval" => 5
      }}}
  """
  def device_authorization(scopes \\ "api repositories") do
    config = Client.config()

    params =
      %{
        "client_id" => @client_id,
        "scope" => scopes
      }

    :mix_hex_api_oauth.device_authorization(config, params)
  end

  @doc """
  Polls the OAuth token endpoint for device authorization completion.

  ## Examples

      iex> Hex.API.OAuth.poll_device_token(device_code)
      {:ok, {200, _headers, %{
        "access_token" => "...",
        "refresh_token" => "...",
        "token_type" => "Bearer",
        "expires_in" => 3600
      }}}
  """
  def poll_device_token(device_code) do
    config = Client.config()

    params = %{
      "grant_type" => "urn:ietf:params:oauth:grant-type:device_code",
      "device_code" => device_code,
      "client_id" => @client_id
    }

    :mix_hex_api_oauth.token(config, params)
  end

  @doc """
  Exchanges a token for a new token with different scopes using RFC 8693 token exchange.

  ## Examples

      iex> Hex.API.OAuth.exchange_token(subject_token, "api:write")
      {:ok, {200, _headers, %{
        "access_token" => "...",
        "refresh_token" => "...",
        "token_type" => "Bearer",
        "expires_in" => 3600
      }}}
  """
  def exchange_token(subject_token, scope) do
    config = Client.config()

    params = %{
      "grant_type" => "urn:ietf:params:oauth:grant-type:token-exchange",
      "subject_token" => subject_token,
      "subject_token_type" => "urn:ietf:params:oauth:token-type:access_token",
      "client_id" => @client_id,
      "scope" => scope
    }

    :mix_hex_api_oauth.token(config, params)
  end

  @doc """
  Refreshes an access token using a refresh token.

  ## Examples

      iex> Hex.API.OAuth.refresh_token(refresh_token)
      {:ok, {200, _headers, %{
        "access_token" => "...",
        "refresh_token" => "...",
        "token_type" => "Bearer",
        "expires_in" => 3600
      }}}
  """
  def refresh_token(refresh_token) do
    config = Client.config()

    params = %{
      "grant_type" => "refresh_token",
      "refresh_token" => refresh_token,
      "client_id" => @client_id
    }

    :mix_hex_api_oauth.token(config, params)
  end

  @doc """
  Revokes an OAuth token (access or refresh token).

  ## Examples

      iex> Hex.API.OAuth.revoke_token(token)
      {:ok, {200, _headers, nil}}
  """
  def revoke_token(token) do
    config = Client.config()

    params = %{
      "token" => token,
      "client_id" => @client_id
    }

    :mix_hex_api_oauth.revoke(config, params)
  end
end
