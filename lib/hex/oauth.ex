defmodule Hex.OAuth do
  @moduledoc false

  @doc """
  Gets the current access token from state.

  Returns `{:ok, token}` if a valid token exists, or `{:error, reason}` otherwise.
  """
  def get_token do
    case Hex.State.get(:oauth_token) do
      nil -> {:error, :no_token}
      %{"access_token" => token} -> {:ok, token}
      _ -> {:error, :invalid_token}
    end
  end

  @doc """
  Stores OAuth token data.

  Since we now use 2FA for write operations, we only store a single token.

  Expected format:
  %{
    "access_token" => "...",
    "refresh_token" => "...",
    "expires_at" => unix_timestamp
  }
  """
  def store_token(token_data) do
    Hex.Config.update([{:"$oauth_token", token_data}])
    Hex.State.put(:oauth_token, token_data)
  end

  @doc """
  Clears all stored OAuth tokens.
  """
  def clear_tokens do
    Hex.Config.remove([:"$oauth_token"])
    Hex.State.put(:oauth_token, nil)
  end

  @doc """
  Checks if we have any OAuth tokens stored.
  """
  def has_tokens? do
    Hex.State.get(:oauth_token) != nil
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
end
