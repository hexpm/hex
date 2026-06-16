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
      {:ok, tokens} -> {:ok, drop_undefined_refresh_token(tokens)}
      other -> other
    end
  end

  # :mix_hex_api_oauth always includes a :refresh_token key, using the atom
  # :undefined when the server didn't return one. Drop it so stored token maps
  # only ever contain a binary refresh token (or no key at all).
  defp drop_undefined_refresh_token(%{refresh_token: refresh} = tokens)
       when refresh in [:undefined, nil] do
    Map.delete(tokens, :refresh_token)
  end

  defp drop_undefined_refresh_token(tokens), do: tokens

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
