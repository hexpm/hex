defmodule Hex.API.OAuth do
  @moduledoc false

  alias Hex.API.Client

  @client_id "78ea6566-89fd-481e-a1d6-7d9d78eacca8"

  @doc false
  def client_id, do: @client_id

  @doc """
  Runs the complete OAuth device authorization flow.

  See `:mix_hex_api_oauth.device_auth_flow/5` for more details.

  ## Examples

      iex> prompt_fn = fn uri, code -> IO.puts("Visit \#{uri} and enter: \#{code}") end
      iex> Hex.API.OAuth.device_auth_flow("api", prompt_fn)
      {:ok, %{access_token: "...", refresh_token: "...", expires_at: 1234567890}}

      iex> Hex.API.OAuth.device_auth_flow("api", prompt_fn, open_browser: true)
      {:ok, %{access_token: "...", refresh_token: "...", expires_at: 1234567890}}
  """
  def device_auth_flow(scopes, prompt_user, opts \\ []) do
    config = Client.config()

    case :mix_hex_api_oauth.device_auth_flow(config, @client_id, scopes, prompt_user, opts) do
      {:ok, tokens} -> {:ok, drop_empty_sso_reauth_required(tokens)}
      other -> other
    end
  end

  # :mix_hex_api_oauth reports "nothing is flagged" as an empty list. A stored
  # token map carries the key only when there is something in it.
  defp drop_empty_sso_reauth_required(%{sso_reauth_required: []} = tokens) do
    Map.delete(tokens, :sso_reauth_required)
  end

  defp drop_empty_sso_reauth_required(tokens), do: tokens

  @doc """
  Requests a URL for authenticating this session against organizations that
  require single sign-on.

  ## Examples

      iex> Hex.API.OAuth.sso_authorization(["acme"])
      {:ok, {201, _headers, %{"verification_uri" => "https://hex.pm/sso/authorize/...",
                              "expires_in" => 600}}}
  """
  def sso_authorization(organizations) do
    config = Client.config()

    Hex.Auth.with_session_api(
      :read,
      config,
      fn config -> :mix_hex_api_oauth.sso_authorization(config, organizations) end,
      auth_inline: false
    )
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
