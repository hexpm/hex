defmodule Hex.Auth do
  @moduledoc false

  @doc """
  Execute a function with API authentication.

  Options:
    * :auth_inline - When true (default), initiates device auth for write ops
                     when no credentials found. When false, returns error.
  """
  def with_api(permission, config, fun, opts \\ []) do
    :mix_hex_cli_auth.with_api(permission, config, fun, opts)
  end

  @doc """
  Execute a function with repository authentication.
  """
  def with_repo(config, fun, opts \\ []) do
    case :mix_hex_cli_auth.with_repo(config, fun, opts) do
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
    config = put_in(config.cli_auth_callbacks.get_auth_config, fn _ -> auth end)
    :mix_hex_cli_auth.with_repo(config, fun, opts)
  end

  @doc false
  @spec callbacks() :: :mix_hex_cli_auth.callbacks()
  def callbacks do
    %{
      get_auth_config: &get_auth_config/1,
      get_oauth_tokens: &get_oauth_tokens/0,
      persist_oauth_tokens: &persist_oauth_tokens/4,
      clear_oauth_tokens: &clear_oauth_tokens/0,
      prompt_otp: &prompt_otp/1,
      get_client_id: &Hex.API.OAuth.client_id/0,
      should_authenticate: &should_authenticate/1
    }
  end

  defp get_auth_config(repo) do
    case {Hex.Repo.fetch_repo(repo), Hex.State.get(:api_key)} do
      {{:ok, config}, nil} -> config
      {{:ok, config}, api_key} -> Map.put_new(config, :api_key, api_key)
      {:error, nil} -> :undefined
      {:error, api_key} -> %{api_key: api_key}
    end
  end

  defp get_oauth_tokens do
    case Hex.State.get(:oauth_token) do
      nil ->
        :error

      %{access_token: access_token, expires_at: expires_at} = token_data ->
        tokens = %{access_token: access_token, expires_at: expires_at}

        tokens =
          if token_data[:refresh_token],
            do: Map.put(tokens, :refresh_token, token_data[:refresh_token]),
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

  # Invoked by hex_cli_auth when the stored global OAuth token is expired and
  # could not be refreshed. Drop it from in-memory state only — keeping the
  # on-disk token, since a refresh can fail transiently — so the rest of this
  # run stops retrying the doomed refresh and proceeds unauthenticated.
  defp clear_oauth_tokens do
    Hex.State.put(:oauth_token, nil)

    Hex.Shell.warn(
      "Your authentication session has expired and could not be refreshed. " <>
        "Continuing without credentials; requests for private resources will fail or " <>
        "prompt for authentication. Run `mix hex.user auth` to re-authenticate"
    )

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

  defp should_authenticate(reason)

  defp should_authenticate(:no_credentials) do
    Hex.Shell.yes?("No authenticated user found. Do you want to authenticate now?")
  end

  defp should_authenticate(:token_refresh_failed) do
    Hex.Shell.yes?("Token refresh failed. Do you want to renew your authentication?")
  end
end
