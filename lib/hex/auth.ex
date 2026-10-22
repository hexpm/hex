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
  Execute a function with API authentication as the stored user session.

  An API key authenticates as itself and cannot stand in for the session, so it
  is left out of the resolution. This is for the endpoints that act on the
  session rather than on behalf of whoever is calling.
  """
  def with_session_api(permission, config, fun, opts \\ []) do
    config = Map.delete(config, :api_key)
    config = put_in(config.cli_auth_callbacks.get_auth_config, fn _repo -> :undefined end)
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

  Satisfying an organization's SSO or 2FA requirements grants scopes absent
  from the current access token. Refreshing retrieves those scopes without
  waiting for the token to expire.
  """
  def refresh_tokens(config) do
    :mix_hex_cli_auth.refresh_tokens(config)
  end

  @doc """
  Authenticate requests to a Hex.pm organization's repository with the token
  that `mix hex.organization auth --key` stored for it. Without an unexpired
  token hex_cli_auth uses the user's session or Workload Identity.

  hex_cli_auth would otherwise use a key stored for the organization, or the
  hexpm repository's key from HEX_REPOS_KEY, before the user's session. A key
  stored by an earlier Hex version and HEX_REPOS_KEY don't authenticate
  organization repositories, so neither is handed to it.
  """
  def put_organization_auth(config, organization, repo_config) do
    auth_config = organization_auth_config(config, organization, repo_config)
    put_in(config.cli_auth_callbacks.get_auth_config, fn _repo -> auth_config end)
  end

  @doc """
  The token `mix hex.organization auth --key` stored for an organization's
  repository, while it hasn't expired.
  """
  def organization_token(%{oauth_token: %{access_token: access_token, expires_at: expires_at}}) do
    if :mix_hex_cli_auth.is_token_expired(expires_at), do: :error, else: {:ok, access_token}
  end

  def organization_token(_repo_config), do: :error

  # A repo_key from get_auth_config is used without checking whether the
  # repository is trusted, so only a trusted repository gets the token.
  defp organization_auth_config(%{trusted: true}, organization, repo_config) do
    case organization_token(repo_config) do
      {:ok, access_token} ->
        %{repo_key: "Bearer #{access_token}"}

      :error ->
        maybe_warn_organization_auth(organization, repo_config)
        :undefined
    end
  end

  defp organization_auth_config(_config, _organization, _repo_config), do: :undefined

  # Warns when the repository has a credential Hex doesn't use, or an expired
  # token, and nothing else will authenticate the request.
  defp maybe_warn_organization_auth(organization, repo_config) do
    reason = organization_auth_reason(organization, repo_config)

    if reason && not Hex.OAuth.has_tokens?() && not Hex.WorkloadIdentity.available?() &&
         Hex.Server.should_warn?({:organization_auth, organization}) do
      Hex.Shell.warn("""
      #{reason}

      For development authenticate as a user with `mix hex.user auth`. For CI \
      authenticate with an organization key right before fetching dependencies:

          mix hex.organization auth #{organization} --key KEY
      """)
    end
  end

  # An organization repository's auth_key is HEX_REPOS_KEY or a key set with
  # `mix hex.repo set hexpm --auth-key`, which it inherits from the hexpm
  # repository, or a key an earlier Hex version stored for it.
  defp organization_auth_reason(organization, repo_config) do
    auth_key = Map.get(repo_config, :auth_key)
    repos_key = Hex.State.fetch!(:repos_key)
    hexpm_key = Hex.State.fetch!(:repos) |> Map.fetch!("hexpm") |> Map.get(:auth_key)

    cond do
      auth_key not in [nil, repos_key, hexpm_key] ->
        "Hex doesn't use the key stored for the #{organization} repository. " <>
          "Remove it with `mix hex.organization deauth #{organization}`."

      repos_key != nil ->
        "Hex doesn't use HEX_REPOS_KEY for organization repositories."

      hexpm_key != nil ->
        "Hex doesn't use the hexpm repository's key for organization repositories."

      Map.has_key?(repo_config, :oauth_token) ->
        "The token for the #{organization} repository has expired."

      true ->
        nil
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
      organization_reauth: &organization_reauth/1,
      get_workload_identity_token: &get_workload_identity_token/1,
      persist_workload_identity_token: &persist_workload_identity_token/2,
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
    # The flagged organizations arrive through the organization_reauth callback, not with
    # the token, so carry them over instead of dropping them on every refresh.
    token_data =
      token_map(access_token, expires_at, refresh_token, Hex.OAuth.organization_reauth_required())

    Hex.OAuth.store_token(token_data)
    :ok
  end

  defp persist_oauth_tokens(repo, access_token, refresh_token, expires_at) do
    token_data = token_map(access_token, expires_at, refresh_token)

    Hex.Config.update_repo(repo, &Map.put(&1, :oauth_token, token_data))

    :ok
  end

  defp token_map(access_token, expires_at, refresh_token, organization_reauth_required \\ []) do
    token_data = %{access_token: access_token, expires_at: expires_at}

    token_data =
      if is_binary(refresh_token),
        do: Map.put(token_data, :refresh_token, refresh_token),
        else: token_data

    put_organization_reauth(token_data, organization_reauth_required)
  end

  # Invoked by hex_cli_auth when the stored global OAuth token is expired and
  # the server answered its refresh with a non-2xx; a refresh that never got an
  # answer does not come through here, which is what lets this say the session
  # is over. Drop it from in-memory state only — keeping the on-disk token,
  # since the server may have been failing rather than refusing — so the rest of
  # this run stops retrying the doomed refresh and proceeds unauthenticated.
  defp clear_oauth_tokens do
    Hex.State.put(:oauth_token, nil)

    Hex.Shell.warn(
      "Your authentication session has expired and could not be refreshed. " <>
        "Continuing without credentials; requests for private resources will fail or " <>
        "prompt for authentication. Run `mix hex.user auth` to re-authenticate " <>
        "or run `mix hex.user deauth` to deauthorize the user on the local machine."
    )

    :ok
  end

  # Invoked by hex_cli_auth after every token grant with outstanding organization
  # authentication requirements. Store them with the token so commands can select
  # the organizations they need, including when reusing a token without refreshing.
  defp organization_reauth(organizations) do
    token_data = Hex.State.get(:oauth_token)

    if is_map(token_data) and
         Map.get(token_data, :organization_reauth_required, []) != organizations do
      Hex.OAuth.store_token(put_organization_reauth(token_data, organizations))
    end

    :ok
  end

  defp put_organization_reauth(token_data, []),
    do: Map.delete(token_data, :organization_reauth_required)

  defp put_organization_reauth(token_data, organizations),
    do: Map.put(token_data, :organization_reauth_required, organizations)

  # The outcome of a repository's Workload Identity exchange, a token or why
  # there is none, is kept in memory only. Tokens expire in 15 minutes and come
  # from the CI job rather than from anything the user configured, so they
  # don't belong in the Hex config.
  defp get_workload_identity_token(repo) do
    Map.get(Hex.State.fetch!(:workload_identity_tokens), repo, :error)
  end

  defp persist_workload_identity_token(repo, result) do
    Hex.State.update!(:workload_identity_tokens, &Map.put(&1, repo, result))
  end

  # A prompt answers :eof when there is nothing on stdin to read, which is what
  # an OTP challenge in CI gets.
  defp prompt_otp(message) do
    case Hex.Shell.prompt(message) do
      otp when is_binary(otp) ->
        otp = String.trim(otp)
        Hex.State.put(:api_otp, otp)
        {:ok, otp}

      _eof ->
        :cancelled
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
