defmodule Hex.Auth do
  @moduledoc false

  @client_id "78ea6566-89fd-481e-a1d6-7d9d78eacca8"

  @doc """
  Execute a function with API authentication.

  Options:
    * :auth_inline - When true (default), initiates device auth for write ops
                     when no credentials found. When false, returns error.
  """
  def with_api(permission, config, fun, opts \\ []) do
    :mix_hex_cli_auth.with_api(callbacks(), permission, config, fun, opts)
  end

  @doc """
  Execute a function with repository authentication.
  """
  def with_repo(config, fun, opts \\ []) do
    case :mix_hex_cli_auth.with_repo(callbacks(), config, fun, opts) do
      {:error, {:auth_error, :oauth_exchange_failed}} ->
        raise "Failed to exchange API key for OAuth token"

      other ->
        other
    end
  end

  @doc """
  Execute a function with preemptive authentication using the provided auth data.
  """
  def with_preemptive_auth(auth, config, fun, opts \\ []) do
    callbacks = Map.put(callbacks(), :get_auth_config, fn _ -> auth end)
    :mix_hex_cli_auth.with_repo(callbacks, config, fun, opts)
  end

  defp callbacks do
    %{
      get_auth_config: &get_auth_config/1,
      get_oauth_tokens: &get_oauth_tokens/0,
      persist_oauth_tokens: &persist_oauth_tokens/4,
      prompt_otp: &prompt_otp/1,
      get_client_id: &get_client_id/0,
      should_authenticate: &should_authenticate/1
    }
  end

  defp get_auth_config(repo) do
    case Hex.State.get(:api_key) do
      nil ->
        case Hex.Repo.fetch_repo(repo) do
          {:ok, config} -> config
          :error -> :undefined
        end

      api_key ->
        %{api_key: api_key}
    end
  end

  defp get_oauth_tokens do
    case Hex.State.get(:oauth_token) do
      nil ->
        :error

      %{"access_token" => access_token, "expires_at" => expires_at} = token_data ->
        tokens = %{access_token: access_token, expires_at: expires_at}

        tokens =
          if token_data["refresh_token"],
            do: Map.put(tokens, :refresh_token, token_data["refresh_token"]),
            else: tokens

        {:ok, tokens}
    end
  end

  defp persist_oauth_tokens(repo, access_token, refresh_token, expires_at)

  defp persist_oauth_tokens(:global, access_token, refresh_token, expires_at) do
    token_data = %{
      access_token: access_token,
      expires_at: expires_at
    }

    token_data =
      if refresh_token,
        do: Map.put(token_data, :refresh_token, refresh_token),
        else: token_data

    Hex.OAuth.store_token(token_data)
    :ok
  end

  defp persist_oauth_tokens(repo, access_token, refresh_token, expires_at) do
    token_data = %{
      access_token: access_token,
      expires_at: expires_at
    }

    token_data =
      if refresh_token,
        do: Map.put(token_data, :refresh_token, refresh_token),
        else: token_data

    repo_config =
      Hex.Repo.get_repo(repo)
      |> Map.put(:oauth_token, token_data)

    Hex.State.fetch!(:repos)
    |> Map.put(repo, repo_config)
    |> Hex.Config.update_repos()

    :ok
  end

  defp prompt_otp(message) do
    case Hex.Shell.prompt(message) do
      nil ->
        :cancelled

      otp ->
        otp = String.trim(otp)
        Hex.State.put(:api_otp, otp)
        {:ok, otp}
    end
  end

  defp get_client_id do
    @client_id
  end

  defp should_authenticate(reason)

  defp should_authenticate(:no_credentials) do
    Hex.Shell.yes?("No authenticated user found. Do you want to authenticate now?")
  end

  defp should_authenticate(:token_refresh_failed) do
    Hex.Shell.info("Token refresh failed. Do you want to renew your authentication?")
  end
end
