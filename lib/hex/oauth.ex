defmodule Hex.OAuth do
  @moduledoc false

  alias Hex.API.OAuth

  @refresh_cache __MODULE__.RefreshCache

  def start_link(_args) do
    Hex.OnceCache.start_link(name: @refresh_cache)
  end

  def child_spec(arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [arg]}
    }
  end

  @doc """
  Retrieves a valid access token.

  Automatically refreshes the token if it's expired.
  Returns {:error, :no_auth} if no tokens are available.

  Since we now use 2FA for write operations, we use a single token for both read and write.

  Uses Hex.OnceCache to ensure only one refresh happens per CLI invocation when multiple
  concurrent requests detect an expired token.

  Options:
    * :prompt_auth - if true, prompts for authentication when refresh fails (default: false)
  """
  def get_token(opts \\ []) do
    # First, check if we have a valid token (read-only, fast path)
    case get_stored_token() do
      nil ->
        {:error, :no_auth}

      token_data ->
        if valid_token?(token_data) do
          {:ok, token_data["access_token"]}
        else
          # Token expired, use OnceCache to ensure only one refresh/auth happens
          # Use infinity timeout because authentication may require user interaction
          Hex.OnceCache.fetch(
            @refresh_cache,
            fn -> do_refresh_or_authenticate(token_data, opts) end,
            timeout: :infinity
          )
        end
    end
  end

  defp do_refresh_or_authenticate(token_data, opts) do
    case do_refresh_token(token_data) do
      {:ok, new_token_data} ->
        store_token(new_token_data)
        {:ok, new_token_data["access_token"]}

      {:error, :refresh_failed} = error ->
        if Keyword.get(opts, :prompt_auth, false) do
          prompt_and_authenticate("Token refresh failed. Please re-authenticate.")
        else
          error
        end

      {:error, :no_refresh_token} = error ->
        if Keyword.get(opts, :prompt_auth, false) do
          prompt_and_authenticate(
            "Access token expired and could not be refreshed. Please re-authenticate."
          )
        else
          error
        end
    end
  end

  defp prompt_and_authenticate(message) do
    Hex.Shell.info(message)

    if Hex.Shell.yes?("No authenticated user found. Do you want to authenticate now?") do
      case Mix.Tasks.Hex.auth() do
        {:ok, token_data} ->
          # Store the new token (auth() already did this, but be explicit)
          # Return the access token
          {:ok, token_data["access_token"]}

        :error ->
          {:error, :auth_failed}
      end
    else
      {:error, :auth_declined}
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
  Clears all stored OAuth tokens and the refresh cache.
  """
  def clear_tokens do
    Hex.Config.remove([:"$oauth_token"])
    Hex.State.put(:oauth_token, nil)
    Hex.OnceCache.clear(@refresh_cache)
  end

  @doc """
  Checks if we have any OAuth tokens stored.
  """
  def has_tokens? do
    get_stored_token() != nil
  end

  @doc """
  Refreshes the stored OAuth token.

  This is primarily for manual refresh operations. Most code should use get_token/0
  which automatically refreshes when needed.
  """
  def refresh_token do
    case get_stored_token() do
      nil ->
        {:error, :no_auth}

      token_data ->
        case do_refresh_token(token_data) do
          {:ok, new_token_data} ->
            # Update the token in state
            store_token(new_token_data)
            {:ok, new_token_data["access_token"]}

          error ->
            error
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

  defp get_stored_token do
    Hex.State.get(:oauth_token)
  end

  defp valid_token?(token_data) do
    case token_data do
      %{"access_token" => token, "expires_at" => expires_at} when is_binary(token) ->
        current_time = System.system_time(:second)
        # Consider token expired if it expires within the next 5 minutes
        expires_at > current_time + 300

      _ ->
        false
    end
  end

  defp do_refresh_token(token_data) do
    if token_data["refresh_token"] do
      case OAuth.refresh_token(token_data["refresh_token"]) do
        {:ok, {200, _, new_token_data}} ->
          # Update the token data with new values
          expires_at = System.system_time(:second) + new_token_data["expires_in"]

          new_token_data =
            new_token_data
            |> Map.put("expires_at", expires_at)
            |> Map.take(["access_token", "refresh_token", "expires_at"])

          {:ok, new_token_data}

        {:ok, {status, _, _error}} when status >= 400 ->
          {:error, :refresh_failed}

        {:error, _reason} ->
          {:error, :refresh_failed}
      end
    else
      # No refresh token available, return error
      {:error, :no_refresh_token}
    end
  end
end
