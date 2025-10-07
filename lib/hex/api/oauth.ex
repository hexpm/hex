defmodule Hex.API.OAuth do
  @moduledoc false

  alias Hex.API.Client

  @client_id "78ea6566-89fd-481e-a1d6-7d9d78eacca8"

  @doc """
  Initiates the OAuth device authorization flow.

  Returns device code, user code, and verification URIs for user authentication.
  Optionally accepts a name parameter to identify the token.

  ## Examples

      iex> Hex.API.OAuth.device_authorization("api")
      {:ok, {200, _headers, %{
        "device_code" => "...",
        "user_code" => "ABCD-1234",
        "verification_uri" => "https://hex.pm/oauth/device",
        "verification_uri_complete" => "https://hex.pm/oauth/device?user_code=ABCD-1234",
        "expires_in" => 600,
        "interval" => 5
      }}}
  """
  def device_authorization(scopes, name \\ nil) do
    config = Client.config()
    opts = if name, do: [name: name], else: []
    :mix_hex_api_oauth.device_authorization(config, @client_id, scopes, opts)
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
    :mix_hex_api_oauth.poll_device_token(config, @client_id, device_code)
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
    :mix_hex_api_oauth.refresh_token(config, @client_id, refresh_token)
  end

  @doc """
  Revokes an OAuth token (access or refresh token).

  ## Examples

      iex> Hex.API.OAuth.revoke_token(token)
      {:ok, {200, _headers, nil}}
  """
  def revoke_token(token) do
    config = Client.config()
    :mix_hex_api_oauth.revoke_token(config, @client_id, token)
  end
end
