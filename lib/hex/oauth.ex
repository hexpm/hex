defmodule Hex.OAuth do
  @moduledoc false

  alias Hex.API.OAuth

  @doc """
  Retrieves a valid access token for the given permission type.

  Automatically refreshes the token if it's expired.
  Returns {:error, :no_auth} if no tokens are available.
  """
  def get_token(permission) when permission in [:read, :write] do
    case get_stored_tokens() do
      nil ->
        {:error, :no_auth}

      tokens ->
        token_data = Map.get(tokens, to_string(permission))

        if token_data && valid_token?(token_data) do
          {:ok, token_data["access_token"]}
        else
          # Try to refresh the token
          refresh_token_if_possible(permission, token_data)
        end
    end
  end

  @doc """
  Stores OAuth tokens for both read and write permissions.

  Expected format:
  %{
    "write" => %{
      "access_token" => "...",
      "refresh_token" => "...",
      "expires_at" => unix_timestamp
    },
    "read" => %{
      "access_token" => "...",
      "refresh_token" => "...",
      "expires_at" => unix_timestamp
    }
  }
  """
  def store_tokens(tokens) do
    Hex.Config.update([{:"$oauth_tokens", tokens}])
    Hex.State.put(:oauth_tokens, tokens)
  end

  @doc """
  Clears all stored OAuth tokens.
  """
  def clear_tokens do
    Hex.Config.remove([:"$oauth_tokens"])
    Hex.State.put(:oauth_tokens, nil)
  end

  @doc """
  Checks if we have any OAuth tokens stored.
  """
  def has_tokens? do
    get_stored_tokens() != nil
  end

  @doc """
  Refreshes a token for the given permission type.
  """
  def refresh_token(permission) when permission in [:read, :write] do
    case get_stored_tokens() do
      nil ->
        {:error, :no_auth}

      tokens ->
        permission_str = to_string(permission)
        token_data = Map.get(tokens, permission_str)

        if token_data && token_data["refresh_token"] do
          case OAuth.refresh_token(token_data["refresh_token"]) do
            {:ok, {200, _, new_token_data}} ->
              # Update the token data with new values
              expires_at = System.system_time(:second) + new_token_data["expires_in"]

              new_token_data =
                new_token_data
                |> Map.put("expires_at", expires_at)
                |> Map.take(["access_token", "refresh_token", "expires_at"])

              # Update stored tokens
              updated_tokens = Map.put(tokens, permission_str, new_token_data)
              store_tokens(updated_tokens)

              {:ok, new_token_data["access_token"]}

            {:ok, {status, _, error}} when status >= 400 ->
              Hex.Shell.debug("Token refresh failed: #{inspect(error)}")
              {:error, :refresh_failed}

            {:error, reason} ->
              Hex.Shell.debug("Token refresh error: #{inspect(reason)}")
              {:error, :refresh_failed}
          end
        else
          {:error, :no_refresh_token}
        end
    end
  end

  @doc """
  Creates token data with expiration time from OAuth response.
  """
  def create_token_data(oauth_response) do
    expires_at = System.system_time(:second) + oauth_response["expires_in"]

    oauth_response
    |> Map.put("expires_at", expires_at)
    |> Map.take(["access_token", "refresh_token", "expires_at"])
  end

  defp get_stored_tokens do
    Hex.State.get(:oauth_tokens)
  end

  defp valid_token?(token_data) do
    case token_data do
      %{"access_token" => token, "expires_at" => expires_at} when is_binary(token) ->
        current_time = System.system_time(:second)
        # Consider token expired if it expires within the next 60 seconds
        expires_at > current_time + 60

      _ ->
        false
    end
  end

  defp refresh_token_if_possible(permission, token_data) do
    case refresh_token(permission) do
      {:ok, access_token} ->
        {:ok, access_token}

      {:error, :no_refresh_token} ->
        # No refresh token available, check if current token is still valid
        case token_data do
          %{"access_token" => token, "expires_at" => expires_at} when is_binary(token) ->
            current_time = System.system_time(:second)

            if expires_at > current_time do
              {:ok, token}
            else
              {:error, :token_expired}
            end

          _ ->
            {:error, :no_auth}
        end

      {:error, :refresh_failed} ->
        # Refresh explicitly failed (network error, invalid refresh token, etc.)
        {:error, :refresh_failed}

      {:error, _other} ->
        # Other refresh errors should also be treated as refresh failures
        {:error, :refresh_failed}
    end
  end
end
