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
      {:ok, tokens} -> {:ok, clean_token_map(tokens)}
      other -> other
    end
  end

  # :mix_hex_api_oauth always includes both keys: :refresh_token as the atom
  # :undefined when the server didn't return one, and :sso_reauth_required as
  # an empty list when no organization is flagged. Drop them so stored token
  # maps only ever contain a binary refresh token and a non-empty organization
  # list, or no key at all.
  defp clean_token_map(tokens) do
    tokens
    |> drop_undefined_refresh_token()
    |> drop_empty_sso_reauth_required()
  end

  defp drop_undefined_refresh_token(%{refresh_token: refresh} = tokens)
       when refresh in [:undefined, nil] do
    Map.delete(tokens, :refresh_token)
  end

  defp drop_undefined_refresh_token(tokens), do: tokens

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

    Hex.Auth.with_api(:read, config, fn config ->
      :mix_hex_api_oauth.sso_authorization(config, organizations)
    end)
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
