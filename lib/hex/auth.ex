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
  Refresh the stored OAuth token now, whether or not it has expired.

  Authenticating a session against an organization's identity provider grants
  scopes the current access token was minted without, and this is how they are
  picked up without waiting the token out.
  """
  def refresh_tokens(config) do
    :mix_hex_cli_auth.refresh_tokens(config)
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
      sso_reauth: &sso_reauth/1,
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
        {:ok, token_map(access_token, expires_at, token_data[:refresh_token])}
    end
  end

  defp persist_oauth_tokens(repo, access_token, refresh_token, expires_at)

  defp persist_oauth_tokens(:global, access_token, refresh_token, expires_at) do
    # The flagged organizations arrive through the sso_reauth callback, not with
    # the token, so carry them over instead of dropping them on every refresh.
    token_data =
      token_map(access_token, expires_at, refresh_token, Hex.OAuth.sso_reauth_required())

    Hex.OAuth.store_token(token_data)
    :ok
  end

  defp persist_oauth_tokens(repo, access_token, refresh_token, expires_at) do
    token_data = token_map(access_token, expires_at, refresh_token)

    repo_config =
      Hex.Repo.get_repo(repo)
      |> Map.put(:oauth_token, token_data)

    Hex.State.fetch!(:repos)
    |> Map.put(repo, repo_config)
    |> Hex.Config.update_repos()

    :ok
  end

  defp token_map(access_token, expires_at, refresh_token, sso_reauth_required \\ []) do
    token_data = %{access_token: access_token, expires_at: expires_at}

    token_data =
      if refresh_token, do: Map.put(token_data, :refresh_token, refresh_token), else: token_data

    put_sso_reauth(token_data, sso_reauth_required)
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
        "prompt for authentication. Run `mix hex.user auth` to re-authenticate" <>
        "or run `mix hex.user deauth` to deauthorize the user on the local machine."
    )

    :ok
  end

  # Invoked by hex_cli_auth after every token grant with the organizations the
  # server says this session has to authenticate through their identity
  # provider for. Store them with the token rather than acting on them: which
  # ones matter depends on what the running command needs, and a later run that
  # reuses this token without refreshing it would otherwise have no idea.
  defp sso_reauth(organizations) do
    token_data = Hex.State.get(:oauth_token)

    if is_map(token_data) and Map.get(token_data, :sso_reauth_required, []) != organizations do
      Hex.OAuth.store_token(put_sso_reauth(token_data, organizations))
    end

    :ok
  end

  defp put_sso_reauth(token_data, []), do: Map.delete(token_data, :sso_reauth_required)

  defp put_sso_reauth(token_data, organizations),
    do: Map.put(token_data, :sso_reauth_required, organizations)

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
