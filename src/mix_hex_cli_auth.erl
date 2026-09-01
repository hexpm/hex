%% Vendored from hex_core v0.19.0 (9ea52a0), do not edit manually

%% @doc
%% Authentication handling with callback functions for build-tool-specific operations.
%%
%% This module provides generic authentication handling that allows both rebar3
%% and Elixir Hex (and future build tools) to share the common auth logic while
%% customizing prompting, persistence, and configuration retrieval.
%%
%% == Callbacks ==
%%
%% Callbacks are provided via the `cli_auth_callbacks' key in the config map.
%% All callbacks below are required unless marked optional:
%%
%% ```
%% #{
%%     %% Auth configuration for a specific repo
%%     get_auth_config => fun((RepoName :: binary()) ->
%%         #{api_key => binary(),
%%           auth_key => binary(),
%%           oauth_exchange => boolean(),
%%           oauth_exchange_url => binary()} | undefined),
%%
%%     %% Global OAuth tokens - storage and retrieval
%%     get_oauth_tokens => fun(() -> {ok, #{access_token := binary(),
%%                                          refresh_token => binary(),
%%                                          expires_at := integer()}} | error),
%%     persist_oauth_tokens => fun((Scope :: global | binary(),
%%                                  AccessToken :: binary(),
%%                                  RefreshToken :: binary() | undefined,
%%                                  ExpiresAt :: integer()) -> ok),
%%
%%     %% Invalidate the stored global OAuth token after the server refused to
%%     %% refresh it (optional). Lets the build tool drop the unusable
%%     %% token so concurrent and subsequent callers stop retrying the doomed
%%     %% refresh, and warn the user. Invoked at most once per resolution, while
%%     %% holding the token-refresh lock.
%%     clear_oauth_tokens => fun(() -> ok),
%%
%%     %% Report the organizations the server says this session has to
%%     %% authenticate against their identity provider for (optional). Called
%%     %% after every token grant that carried a readable set, with the empty
%%     %% list when there are none, so the build tool always holds the current
%%     %% set. It is not told which of them the running command needs; deciding
%%     %% that is the build tool's job.
%%     sso_reauth => fun(([binary()]) -> ok),
%%
%%     %% User interaction
%%     prompt_otp => fun((Message :: binary()) -> {ok, OtpCode :: binary()} | cancelled),
%%     should_authenticate => fun((Reason :: no_credentials | token_refresh_failed) -> boolean()),
%%
%%     %% OAuth client configuration
%%     get_client_id => fun(() -> binary())
%% }
%% '''
%%
%% == Auth Resolution Order ==
%%
%% For API calls:
%% <ol>
%% <li>Per-repo `api_key' from config (with optional OAuth exchange for hex.pm)</li>
%% <li>Parent repo `api_key' (for "hexpm:org" organizations)</li>
%% <li>Global OAuth token (refreshed if expired)</li>
%% <li>Device auth flow (for write operations only)</li>
%% </ol>
%%
%% For repo calls:
%% <ol>
%% <li>Per-repo `auth_key' with optional OAuth exchange (default true for hex.pm)</li>
%% <li>Parent repo `auth_key'</li>
%% <li>Global OAuth token</li>
%% </ol>
%%
%% == OAuth Exchange ==
%%
%% For hex.pm URLs, `api_key' and `auth_key' are exchanged for short-lived OAuth
%% tokens via the client credentials grant. This behavior can be controlled per-repo
%% via the `oauth_exchange' option in the repo config (defaults to `true' for hex.pm).
%%
%% == Auth Context ==
%%
%% Internally, authentication resolution tracks context via `auth_context()':
%% <ul>
%% <li>`has_refresh_token' - Whether token refresh is possible on 401</li>
%% </ul>
%%
%% == Token Format ==
%%
%% OAuth access tokens are automatically prefixed with `<<"Bearer ">>' when used
%% as `api_key' or `repo_key' in the config.
-module(mix_hex_cli_auth).

-export([
    with_api/3,
    with_api/4,
    with_repo/2,
    with_repo/3,
    resolve_api_auth/2,
    resolve_repo_auth/1,
    refresh_tokens/1,
    is_token_expired/1
]).

-export_type([
    callbacks/0,
    permission/0,
    auth_error/0,
    auth_context/0,
    repo_auth_config/0,
    auth_prompt_reason/0,
    opts/0
]).

%% 5 minute buffer before expiry
-define(EXPIRY_BUFFER_SECONDS, 300).

%% Maximum OTP retry attempts
-define(MAX_OTP_RETRIES, 3).

%% Maximum times a 401 that says the token expired is answered by renewing the
%% credential and running the request again.
-define(MAX_TOKEN_RETRIES, 1).

%% Both ways a refresh can leave us without a usable token: the server refused
%% the refresh token, or the refresh got no usable answer.
-define(IS_REFRESH_FAILURE(Reason),
    (Reason =:= token_refresh_failed orelse Reason =:= token_refresh_unavailable)
).

-type permission() :: read | write.

-type callbacks() :: #{
    get_auth_config := fun((RepoName :: binary()) -> repo_auth_config() | undefined),
    get_oauth_tokens := fun(() -> {ok, oauth_tokens()} | error),
    persist_oauth_tokens := fun(
        (
            Scope :: global | binary(),
            AccessToken :: binary(),
            RefreshToken :: binary() | undefined,
            ExpiresAt :: integer()
        ) -> ok
    ),
    clear_oauth_tokens => fun(() -> ok),
    sso_reauth => fun((Organizations :: [binary()]) -> ok),
    prompt_otp := fun((Message :: binary()) -> {ok, OtpCode :: binary()} | cancelled),
    should_authenticate := fun((Reason :: auth_prompt_reason()) -> boolean()),
    get_client_id := fun(() -> binary())
}.

-type auth_prompt_reason() ::
    no_credentials
    | token_refresh_failed.

-type repo_auth_config() :: #{
    api_key => binary(),
    repo_key => binary(),
    auth_key => binary(),
    oauth_token => oauth_tokens()
}.

-type oauth_tokens() :: #{
    access_token := binary(),
    refresh_token => binary(),
    expires_at := integer()
}.

-type auth_error() ::
    {auth_error, no_credentials}
    | {auth_error, auth_declined}
    | {auth_error, otp_cancelled}
    | {auth_error, otp_max_retries}
    %% The server refused the refresh token: 400 or 401
    | {auth_error, token_refresh_failed}
    %% The refresh got no usable answer: DNS, connect, timeout, TLS, a 429 or a
    %% 5xx, or a 200 whose body is not a token response
    | {auth_error, token_refresh_unavailable}
    | {auth_error, device_auth_timeout}
    | {auth_error, device_auth_denied}
    | {auth_error, oauth_exchange_failed}
    | {auth_error, term()}.

-type auth_context() :: #{
    has_refresh_token => boolean()
}.

%% How much of each retry budget a request has already spent.
-type retries() :: #{
    otp := non_neg_integer(),
    otp_error := invalid_totp | undefined,
    token := non_neg_integer()
}.

-type opts() :: [
    {optional, boolean()}
    | {auth_inline, boolean()}
    | {oauth_open_browser, boolean()}
].

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Execute a function with API authentication.
%%
%% Equivalent to `with_api(Permission, Config, Fun, [])'.
%%
%% @see with_api/4
-spec with_api(permission(), mix_hex_core:config(), fun((mix_hex_core:config()) -> Result)) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_api(Permission, BaseConfig, Fun) ->
    with_api(Permission, BaseConfig, Fun, []).

%% @doc
%% Execute a function with API authentication.
%%
%% Resolves credentials in this order:
%% <ol>
%% <li>Per-repo `api_key' from config (with optional OAuth exchange for hex.pm)</li>
%% <li>Parent repo `api_key' (for "hexpm:org" organizations)</li>
%% <li>Global OAuth token (refreshed if expired)</li>
%% <li>Device auth flow (when `should_authenticate' callback returns true)</li>
%% </ol>
%%
%% On 401 responses, handles OTP prompts and token refresh automatically.
%%
%% The repository name is taken from the config (`repo_name' or `repo_organization').
%%
%% Callbacks are taken from the `cli_auth_callbacks' key in the config map.
%%
%% Options:
%% <ul>
%% <li>`optional' - When `true', if no credentials are found, executes the function
%%     without authentication first. If the server returns 401, triggers auth
%%     (respecting `auth_inline'). When `false' (default), missing credentials
%%     immediately triggers the `should_authenticate' callback.</li>
%% <li>`auth_inline' - When `true' (default), prompts the user via `should_authenticate'
%%     callback when authentication is needed. When `false', returns
%%     `{error, {auth_error, no_credentials}}' instead of prompting.</li>
%% <li>`oauth_open_browser' - When `true' (default), automatically opens the browser
%%     during device auth flow. When `false', only prints the URL for the user.</li>
%% </ul>
%%
%% Example:
%% ```
%% mix_hex_cli_auth:with_api(write, Config, fun(C) ->
%%     mix_hex_api_release:publish(C, Tarball)
%% end, [{optional, false}, {auth_inline, true}]).
%% '''
-spec with_api(
    permission(),
    mix_hex_core:config(),
    fun((mix_hex_core:config()) -> Result),
    opts()
) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_api(Permission, BaseConfig, Fun, Opts) ->
    Optional = proplists:get_value(optional, Opts, false),
    AuthInline = proplists:get_value(auth_inline, Opts, true),
    case resolve_api_auth(Permission, BaseConfig) of
        {ok, ApiKey, AuthContext} ->
            Config = BaseConfig#{api_key => ApiKey},
            execute_with_retry(Config, Fun, AuthContext, initial_retries(), Opts);
        {error, no_auth} when Optional =:= true ->
            %% Auth is optional, try without credentials first
            execute_optional_with_retry(api, BaseConfig, Fun, AuthInline, Opts);
        {error, no_auth} when AuthInline =:= true ->
            %% No auth found, ask user if they want to authenticate
            maybe_authenticate_and_retry(
                api, BaseConfig, Fun, no_credentials, initial_retries(), Opts
            );
        {error, no_auth} ->
            %% auth_inline is false, just return error
            {error, {auth_error, no_credentials}};
        {error, {auth_error, Reason}} when Optional =:= true, ?IS_REFRESH_FAILURE(Reason) ->
            %% Token refresh failed but auth is optional, fall back to no credentials
            execute_optional_with_retry(api, BaseConfig, Fun, AuthInline, Opts);
        {error, {auth_error, token_refresh_failed}} when AuthInline =:= true ->
            %% The server refused the refresh token, which leaves us with no
            %% usable token, same as having none, so it gets the same offer to
            %% authenticate. Without this the caller that asked to be prompted up
            %% front is told to run mix hex.user auth instead of being asked.
            %%
            %% Not token_refresh_unavailable: there the refresh got no answer at
            %% all, so the token may well still be good and the network is what
            %% is wrong. Offering a device flow that needs the same network is no
            %% help.
            maybe_authenticate_and_retry(
                api, BaseConfig, Fun, token_refresh_failed, initial_retries(), Opts
            );
        {error, _} = Error ->
            Error
    end.

%% @doc
%% Execute a function with repository authentication.
%%
%% Equivalent to `with_repo(Config, Fun, [])'.
%%
%% @see with_repo/3
-spec with_repo(mix_hex_core:config(), fun((mix_hex_core:config()) -> Result)) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_repo(BaseConfig, Fun) ->
    with_repo(BaseConfig, Fun, []).

%% @doc
%% Execute a function with repository authentication.
%%
%% Resolves credentials in this order:
%% <ol>
%% <li>`repo_key' in config - passthrough</li>
%% <li>`repo_key' from `get_auth_config' callback - passthrough</li>
%% <li>`auth_key' from `get_auth_config' when `trusted' is true and `oauth_exchange' is true - exchange for OAuth token</li>
%% <li>`auth_key' from `get_auth_config' when `trusted' is true - use directly</li>
%% <li>Global OAuth token from `get_oauth_tokens' callback for Hex.pm repositories</li>
%% <li>No auth when `optional' is true (with retry on 401)</li>
%% <li>Prompt via `should_authenticate' when `auth_inline' is true</li>
%% </ol>
%%
%% A resolved token the server answers with a `token_expired' 401 is renewed at
%% its source (a per-repo token is exchanged again, the global token is
%% refreshed) and the request is run once more.
%%
%% The repository name is taken from the config (`repo_name' or `repo_organization').
%%
%% Callbacks are taken from the `cli_auth_callbacks' key in the config map.
%%
%% Options:
%% <ul>
%% <li>`optional' - When `true' (default), proceeds without auth if none found; retries with auth on 401.</li>
%% <li>`auth_inline' - When `true', prompts user via `should_authenticate' callback. Default is `false'.</li>
%% <li>`oauth_open_browser' - When `true' (default), automatically opens the browser
%%     during device auth flow. When `false', only prints the URL for the user.</li>
%% </ul>
%%
%% Example:
%% ```
%% mix_hex_cli_auth:with_repo(Config, fun(C) ->
%%     mix_hex_repo:get_tarball(C, <<"ecto">>, <<"3.0.0">>)
%% end).
%% '''
-spec with_repo(mix_hex_core:config(), fun((mix_hex_core:config()) -> Result), opts()) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_repo(BaseConfig, Fun, Opts) ->
    Optional = proplists:get_value(optional, Opts, true),
    AuthInline = proplists:get_value(auth_inline, Opts, false),
    case resolve_repo_auth(BaseConfig) of
        {ok, RepoKey, _AuthContext} when is_binary(RepoKey) ->
            execute_repo_with_retry(BaseConfig, Fun, RepoKey);
        no_auth when Optional =:= true ->
            %% Auth is optional, try without credentials first
            execute_optional_with_retry(repo, BaseConfig, Fun, AuthInline, Opts);
        no_auth when AuthInline =:= true ->
            %% No auth found, ask user if they want to authenticate
            maybe_authenticate_and_retry(
                repo, BaseConfig, Fun, no_credentials, initial_retries(), Opts
            );
        no_auth ->
            %% auth_inline is false, return error
            {error, {auth_error, no_credentials}};
        {error, {auth_error, Reason}} when Optional =:= true, ?IS_REFRESH_FAILURE(Reason) ->
            %% Token refresh failed but auth is optional, fall back to no credentials
            execute_optional_with_retry(repo, BaseConfig, Fun, AuthInline, Opts);
        {error, _} = Error ->
            Error
    end.

%% @private
%% Extract repository name from config.
-spec repo_name(mix_hex_core:config()) -> binary().
repo_name(#{repo_name := Name, repo_organization := Org}) when is_binary(Name) and is_binary(Org) ->
    <<Name/binary, ":", Org/binary>>;
repo_name(#{repo_name := Name}) when is_binary(Name) -> Name;
repo_name(_) ->
    <<"hexpm">>.

%% @private
%% Ask user if they want to authenticate, and if yes, initiate device auth, then
%% run the request with what that produced.
%%
%% Kind says which credential the retried request needs: `api' takes the token
%% as api_key, `repo' resolves repository auth and takes it as repo_key.
%%
%% Acquiring the credential is serialized with a global lock so concurrent
%% callers don't each trigger their own device auth flow. The first caller to
%% acquire the lock runs device auth and persists the resulting token;
%% subsequent callers re-check for an existing (now-valid) token inside the lock
%% and reuse it instead of re-authenticating. The request runs outside the lock.
%% A 401 brings it back here, and global:trans/4 on a lock this process already
%% holds does not take a second reference: the inner transaction's exit deletes
%% the lock entry while the outer one is still running.
maybe_authenticate_and_retry(Kind, BaseConfig, Fun, Reason, Retries, Opts) ->
    Credential = global:trans(
        {{?MODULE, device_auth}, self()},
        fun() ->
            acquire_credential(Kind, BaseConfig, Reason, Opts)
        end,
        [node()],
        infinity
    ),
    case Credential of
        {ok, Key, AuthContext} ->
            execute_authenticated(Kind, BaseConfig, Fun, Key, AuthContext, Retries, Opts);
        {error, _} = Error ->
            Error
    end.

%% @private
%% Another caller may have authenticated while we waited for the lock. Re-resolve
%% and, if we get a token that differs from the one we arrived with (none when
%% credentials were missing; the rejected one on token_refresh_failed), reuse it
%% instead of prompting again. Otherwise proceed to prompt + device auth.
acquire_credential(api, BaseConfig, Reason, Opts) ->
    CurrentApiKey = maps:get(api_key, BaseConfig, undefined),
    case resolve_api_auth(write, BaseConfig) of
        {ok, ApiKey, AuthContext} when ApiKey =/= CurrentApiKey ->
            {ok, ApiKey, AuthContext};
        _ ->
            prompt_and_device_auth(api, BaseConfig, Reason, Opts)
    end;
acquire_credential(repo, BaseConfig, Reason, Opts) ->
    CurrentRepoKey = maps:get(repo_key, BaseConfig, undefined),
    case resolve_repo_auth(BaseConfig) of
        {ok, RepoKey, AuthContext} when is_binary(RepoKey), RepoKey =/= CurrentRepoKey ->
            {ok, RepoKey, AuthContext};
        _ ->
            prompt_and_device_auth(repo, BaseConfig, Reason, Opts)
    end.

%% @private
prompt_and_device_auth(Kind, BaseConfig, Reason, Opts) ->
    case call_callback(BaseConfig, should_authenticate, [Reason]) of
        true ->
            case device_auth(BaseConfig, <<"api repositories">>, Opts) of
                {ok, Tokens} ->
                    authenticated_credential(Kind, BaseConfig, Tokens);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {auth_error, auth_declined}}
    end.

%% @private
%% The credential device auth just produced. The token is the user's API token;
%% what a repository request needs is repository auth, which the token may only
%% be one input to, so resolve that instead of reusing the API-shaped one.
authenticated_credential(api, _BaseConfig, #{access_token := AccessToken} = Tokens) ->
    {ok, <<"Bearer ", AccessToken/binary>>, #{has_refresh_token => has_refresh_token(Tokens)}};
authenticated_credential(repo, BaseConfig, _Tokens) ->
    case resolve_repo_auth(BaseConfig) of
        {ok, RepoKey, AuthContext} when is_binary(RepoKey) ->
            {ok, RepoKey, AuthContext};
        no_auth ->
            {error, {auth_error, no_credentials}};
        {error, _} = Error ->
            Error
    end.

%% @private
execute_authenticated(api, BaseConfig, Fun, ApiKey, AuthContext, Retries, Opts) ->
    execute_with_retry(BaseConfig#{api_key => ApiKey}, Fun, AuthContext, Retries, Opts);
execute_authenticated(repo, BaseConfig, Fun, RepoKey, _AuthContext, _Retries, _Opts) ->
    execute_repo_with_retry(BaseConfig, Fun, RepoKey).

%% @private
%% Execute function without auth, but retry with auth if we get a 401.
execute_optional_with_retry(Kind, BaseConfig, Fun, AuthInline, Opts) ->
    case Fun(BaseConfig) of
        {ok, {401, _Headers, _Body}} when AuthInline =:= true ->
            %% Got 401, need auth - ask user if they want to authenticate
            maybe_authenticate_and_retry(
                Kind, BaseConfig, Fun, no_credentials, initial_retries(), Opts
            );
        {ok, {401, _Headers, _Body}} ->
            %% Got 401 but auth_inline is false, return error
            {error, {auth_error, no_credentials}};
        Other ->
            Other
    end.

%% @private
%% Run a repository request with a resolved token. A 401 that says the token
%% expired is answered by renewing the credential at its source and running the
%% request once more; a second 401 is the caller's to handle.
execute_repo_with_retry(BaseConfig, Fun, RepoKey) ->
    case Fun(BaseConfig#{repo_key => RepoKey}) of
        {ok, {401, Headers, _Body}} = Response ->
            case detect_auth_error(Headers) of
                token_expired ->
                    renew_repo_auth_and_retry(BaseConfig, Fun, RepoKey, Response);
                _Other ->
                    Response
            end;
        Other ->
            Other
    end.

%% @private
renew_repo_auth_and_retry(BaseConfig, Fun, RepoKey, Response) ->
    case resolve_repo_auth(BaseConfig, true) of
        {ok, NewRepoKey, _AuthContext} when is_binary(NewRepoKey), NewRepoKey =/= RepoKey ->
            Fun(BaseConfig#{repo_key => NewRepoKey});
        _Other ->
            Response
    end.

%% @doc
%% Refreshes the stored global OAuth token now, whether or not it has expired.
%%
%% What a token carries can change without it expiring: authenticating a
%% session against an organization's identity provider grants scopes the
%% current access token was minted without. This is how a build tool picks
%% those up rather than waiting out the access token.
-spec refresh_tokens(mix_hex_core:config()) -> ok | {error, auth_error()}.
refresh_tokens(Config) ->
    Refresh = fun(Tokens) ->
        %% A failed refresh leaves the stored token in place; the caller warns
        %% and continues with it.
        case maybe_refresh_token_with_context(Config, Tokens) of
            {ok, _BearerToken, _AuthContext} -> ok;
            {error, _Reason} = Error -> Error
        end
    end,
    case with_token_refresh_lock(Config, Refresh) of
        error -> {error, {auth_error, no_credentials}};
        Result -> Result
    end.

%% @private
%% Check if a token is expired (within 5 minute buffer).
-spec is_token_expired(integer()) -> boolean().
is_token_expired(ExpiresAt) ->
    Now = erlang:system_time(second),
    ExpiresAt - Now < ?EXPIRY_BUFFER_SECONDS.

%%====================================================================
%% Internal functions - Device Auth
%%====================================================================

%% @private
%% Initiate OAuth device authorization flow.
%% Prompts user, optionally opens the browser for user authentication,
%% polls for token completion, and persists tokens via callback on success.
-spec device_auth(mix_hex_core:config(), binary(), opts()) ->
    {ok, oauth_tokens()} | {error, auth_error()}.
device_auth(Config, Scope, Opts) ->
    ClientId = call_callback(Config, get_client_id, []),
    OpenBrowser = proplists:get_value(oauth_open_browser, Opts, true),
    PromptUser = fun(VerificationUri, UserCode) ->
        io:format("Open ~ts in your browser and enter code: ~ts~n", [VerificationUri, UserCode])
    end,
    FlowOpts = [{open_browser, OpenBrowser}],
    case mix_hex_api_oauth:device_auth_flow(Config, ClientId, Scope, PromptUser, FlowOpts) of
        {ok, Response} ->
            %% sso_reauth_required reaches the build tool through the sso_reauth
            %% callback rather than with the tokens. The response carries no key
            %% when the server sent a set that could not be read.
            Tokens = maps:without([sso_reauth_required], Response),
            ok = persist_tokens(Config, global, Tokens),
            report_sso_reauth(Config, maps:find(sso_reauth_required, Response)),
            {ok, Tokens};
        {error, timeout} ->
            {error, {auth_error, device_auth_timeout}};
        {error, {access_denied, _Status, _Body}} ->
            {error, {auth_error, device_auth_denied}};
        {error, {device_auth_failed, _Status, _Body} = Reason} ->
            {error, {auth_error, Reason}};
        {error, {poll_failed, _Status, _Body} = Reason} ->
            {error, {auth_error, Reason}};
        {error, Reason} ->
            {error, {auth_error, Reason}}
    end.

%%====================================================================
%% Internal functions - Auth Resolution
%%====================================================================

%% @private
-spec resolve_api_auth(permission(), mix_hex_core:config()) ->
    {ok, binary(), auth_context()} | {error, no_auth} | {error, auth_error()}.
resolve_api_auth(_Permission, #{api_key := ApiKey}) when is_binary(ApiKey) ->
    %% api_key already in config, pass through directly
    {ok, ApiKey, #{has_refresh_token => false}};
resolve_api_auth(_Permission, Config) ->
    RepoName = repo_name(Config),
    %% 1. Check per-repo api_key
    case call_callback(Config, get_auth_config, [RepoName]) of
        #{api_key := ApiKey} when is_binary(ApiKey) ->
            {ok, ApiKey, #{has_refresh_token => false}};
        _ ->
            %% 2. Check parent repo (for "hexpm:org" organizations)
            case get_parent_repo_key(Config, RepoName, api_key) of
                {ok, ApiKey} ->
                    {ok, ApiKey, #{has_refresh_token => false}};
                error ->
                    %% 3. Try global OAuth token
                    resolve_oauth_token_with_context(Config, false)
            end
    end.

%% @private
%% Resolve repo auth credentials in this order:
%% 0. repo_key in config => passthrough
%% 1. repo_key from get_auth_config => passthrough
%% 2. trusted + auth_key + oauth_exchange => exchange for OAuth token
%% 3. trusted + auth_key => use directly
%% 4. trusted Hex.pm or child repository + global OAuth tokens => use those
%% 5. Fallthrough to no_auth (handled by with_repo/3 for optional/auth_inline)
-spec resolve_repo_auth(mix_hex_core:config()) ->
    {ok, binary(), auth_context()} | no_auth | {error, auth_error()}.
resolve_repo_auth(Config) ->
    resolve_repo_auth(Config, false).

%% @private
%% Renew says the credential we already have was rejected, so a stored token
%% that has not run out of time is exchanged or refreshed anyway.
resolve_repo_auth(#{repo_key := RepoKey}, _Renew) when is_binary(RepoKey) ->
    %% repo_key already in config, pass through directly
    {ok, RepoKey, #{has_refresh_token => false}};
resolve_repo_auth(Config, Renew) ->
    RepoName = repo_name(Config),
    global:trans(
        {{?MODULE, repo, RepoName}, self()},
        fun() ->
            do_resolve_repo_auth(RepoName, RepoName, Config, Renew)
        end,
        [node()],
        infinity
    ).

do_resolve_repo_auth(RepoName, LookupRepo, Config, Renew) ->
    Trusted = maps:get(trusted, Config, false),
    OAuthExchange = maps:get(oauth_exchange, Config, false),
    case call_callback(Config, get_auth_config, [LookupRepo]) of
        #{repo_key := RepoKey} when is_binary(RepoKey) ->
            %% 1. repo_key from get_auth_config => passthrough
            {ok, RepoKey, #{has_refresh_token => false}};
        #{oauth_token := OAuthToken, auth_key := AuthKey} when
            is_binary(AuthKey) and OAuthExchange, Trusted
        ->
            %% 2. trusted + oauth_token + auth_key + oauth_exchange => use/refresh existing token
            resolve_repo_oauth_token(RepoName, Config, AuthKey, OAuthToken, Renew);
        #{auth_key := AuthKey} when is_binary(AuthKey) and OAuthExchange, Trusted ->
            %% 3. trusted + auth_key + oauth_exchange => exchange for new OAuth token
            exchange_for_oauth_token(RepoName, Config, AuthKey, <<"repositories">>);
        #{auth_key := AuthKey} when is_binary(AuthKey), Trusted ->
            %% 4. trusted + auth_key => use directly
            {ok, AuthKey, #{has_refresh_token => false}};
        _ when Trusted ->
            %% 5. Check parent repo (for "hexpm:org" organizations)
            case binary:split(LookupRepo, <<":">>) of
                [ParentName, _OrgName] ->
                    do_resolve_repo_auth(RepoName, ParentName, Config, Renew);
                _ ->
                    %% 6. trusted Hex.pm or child repository + global OAuth tokens => use those
                    resolve_global_oauth_for_repo(RepoName, Config, Renew)
            end;
        _ ->
            %% 7. Not trusted, no auth
            no_auth
    end.

%% @private
resolve_global_oauth_for_repo(<<"hexpm">>, Config, Renew) ->
    resolve_global_oauth_for_repo(Config, Renew);
resolve_global_oauth_for_repo(<<"hexpm:", _/binary>>, Config, Renew) ->
    resolve_global_oauth_for_repo(Config, Renew);
resolve_global_oauth_for_repo(_RepoName, _Config, _Renew) ->
    no_auth.

resolve_global_oauth_for_repo(Config, Renew) ->
    case resolve_oauth_token_with_context(Config, Renew) of
        {ok, Token, AuthContext} ->
            {ok, Token, AuthContext};
        {error, no_auth} ->
            no_auth;
        {error, _} = Error ->
            Error
    end.

%% @private
%% Resolve repo OAuth token: use if valid, re-exchange if expiring or rejected.
resolve_repo_oauth_token(
    RepoName,
    Config,
    AuthKey,
    #{access_token := AccessToken, expires_at := ExpiresAt},
    Renew
) ->
    case Renew orelse is_token_expired(ExpiresAt) of
        false ->
            %% Token is still valid, use it
            BearerToken = <<"Bearer ", AccessToken/binary>>,
            {ok, BearerToken, #{has_refresh_token => false}};
        true ->
            %% Token expired, do a new exchange
            exchange_for_oauth_token(RepoName, Config, AuthKey, <<"repositories">>)
    end.

%% @private
%% Exchange api_key/auth_key for OAuth token via client credentials grant.
%% Persists the token with the repo name for per-repo token storage.
exchange_for_oauth_token(RepoName, Config, AuthKey, Scope) ->
    ClientId = call_callback(Config, get_client_id, []),
    ExchangeConfig =
        case maps:get(oauth_exchange_url, Config, undefined) of
            undefined -> Config;
            OAuthUrl -> Config#{api_url => OAuthUrl}
        end,
    case mix_hex_api_oauth:client_credentials_token(ExchangeConfig, ClientId, AuthKey, Scope) of
        {ok, {200, _, #{<<"access_token">> := AccessToken, <<"expires_in">> := ExpiresIn}}} ->
            Tokens = #{
                access_token => AccessToken,
                expires_at => erlang:system_time(second) + ExpiresIn
            },
            ok = persist_tokens(Config, RepoName, Tokens),
            BearerToken = <<"Bearer ", AccessToken/binary>>,
            {ok, BearerToken, #{has_refresh_token => false}};
        {ok, {_Status, _, _Body}} ->
            {error, {auth_error, oauth_exchange_failed}};
        {error, _} ->
            {error, {auth_error, oauth_exchange_failed}}
    end.

%% @private
get_parent_repo_key(Config, RepoName, KeyType) ->
    case binary:split(RepoName, <<":">>) of
        [ParentName, _OrgName] ->
            case call_callback(Config, get_auth_config, [ParentName]) of
                #{KeyType := Key} when is_binary(Key) ->
                    {ok, Key};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% @private
%% Resolve OAuth token with global lock to prevent concurrent refresh attempts.
%% Renew refreshes a token that has not run out of time, for when the server
%% has rejected it anyway.
resolve_oauth_token_with_context(Config, Renew) ->
    Resolve = fun(#{access_token := AccessToken, expires_at := ExpiresAt} = Tokens) ->
        case Renew orelse is_token_expired(ExpiresAt) of
            true ->
                refresh_or_clear(Config, Tokens);
            false ->
                BearerToken = <<"Bearer ", AccessToken/binary>>,
                {ok, BearerToken, #{has_refresh_token => has_refresh_token(Tokens)}}
        end
    end,
    case with_token_refresh_lock(Config, Resolve) of
        error -> {error, no_auth};
        Result -> Result
    end.

%% @private
%% Fetch the stored global tokens and hand them to Fun under the token-refresh
%% lock, so concurrent callers do not each refresh the same token. Returns
%% `error' without calling Fun when no tokens are stored.
with_token_refresh_lock(Config, Fun) ->
    global:trans(
        {{?MODULE, token_refresh}, self()},
        fun() ->
            case call_callback(Config, get_oauth_tokens, []) of
                {ok, Tokens} -> Fun(Tokens);
                error -> error
            end
        end,
        [node()],
        infinity
    ).

%% @private
%% Refresh an expired global token; if the server rejected the refresh token,
%% invalidate the stored token via the optional clear_oauth_tokens callback.
%% This runs inside the token_refresh lock, so the unusable token is dropped
%% exactly once and the callers serialized behind the lock re-read it as absent
%% instead of each retrying the doomed refresh against the server. A refresh
%% that got no usable answer says nothing about the token, so it is kept.
refresh_or_clear(Config, Tokens) ->
    case maybe_refresh_token_with_context(Config, Tokens) of
        {ok, _Bearer, _Ctx} = Ok ->
            Ok;
        {error, {auth_error, token_refresh_failed}} = Error ->
            maybe_call_callback(Config, clear_oauth_tokens, []),
            Error;
        {error, _} = Error ->
            Error
    end.

%% @private
%% Only 400 and 401 are the server refusing the refresh token, which is what
%% makes the stored credential dead. A 429, a 5xx and a 200 whose body cannot be
%% read all leave the refresh token as good as it was.
maybe_refresh_token_with_context(Config, #{refresh_token := RefreshToken}) when
    is_binary(RefreshToken)
->
    ClientId = call_callback(Config, get_client_id, []),
    case mix_hex_api_oauth:refresh_token(Config, ClientId, RefreshToken) of
        {ok,
            {200, _,
                #{
                    <<"access_token">> := NewAccessToken,
                    <<"expires_in">> := ExpiresIn
                } = TokenResponse}} when
            is_binary(NewAccessToken), is_integer(ExpiresIn)
        ->
            NewTokens = #{
                access_token => NewAccessToken,
                refresh_token => new_refresh_token(TokenResponse, RefreshToken),
                expires_at => erlang:system_time(second) + ExpiresIn
            },
            ok = persist_tokens(Config, global, NewTokens),
            report_sso_reauth(Config, mix_hex_api_oauth:sso_reauth_required(TokenResponse)),
            BearerToken = <<"Bearer ", NewAccessToken/binary>>,
            {ok, BearerToken, #{has_refresh_token => has_refresh_token(NewTokens)}};
        {ok, {Status, _, _Body}} when Status =:= 400; Status =:= 401 ->
            {error, {auth_error, token_refresh_failed}};
        {ok, {_Status, _, _Body}} ->
            {error, {auth_error, token_refresh_unavailable}};
        {error, _Reason} ->
            {error, {auth_error, token_refresh_unavailable}}
    end;
maybe_refresh_token_with_context(_Config, _Tokens) ->
    {error, {auth_error, token_refresh_failed}}.

%% @private
%% A refresh that does not rotate the refresh token keeps the one we sent.
new_refresh_token(#{<<"refresh_token">> := RefreshToken}, _CurrentRefreshToken) when
    is_binary(RefreshToken)
->
    RefreshToken;
new_refresh_token(_TokenResponse, CurrentRefreshToken) ->
    CurrentRefreshToken.

%% @private
%% Whether these tokens can be refreshed.
-spec has_refresh_token(oauth_tokens()) -> boolean().
has_refresh_token(Tokens) ->
    is_binary(maps:get(refresh_token, Tokens, undefined)).

%% @private
%% The one place tokens are handed to the build tool for storage. A token map
%% without a refresh token is persisted as `undefined', which is what the
%% persist_oauth_tokens callback documents for "there is none".
-spec persist_tokens(mix_hex_core:config(), global | binary(), oauth_tokens()) -> ok.
persist_tokens(Config, Scope, #{access_token := AccessToken, expires_at := ExpiresAt} = Tokens) ->
    RefreshToken = maps:get(refresh_token, Tokens, undefined),
    ok = call_callback(Config, persist_oauth_tokens, [
        Scope, AccessToken, RefreshToken, ExpiresAt
    ]).

%%====================================================================
%% Internal functions - Retry Logic
%%====================================================================

%% @private
-spec initial_retries() -> retries().
initial_retries() ->
    #{otp => 0, otp_error => undefined, token => 0}.

%% @private
execute_with_retry(Config, Fun, AuthContext, Retries, Opts) ->
    case Fun(Config) of
        {error, otp_required} ->
            handle_otp_retry(
                Config, Fun, AuthContext, Retries, <<"Enter OTP code:">>, Opts
            );
        {error, invalid_totp} ->
            handle_otp_retry(
                Config,
                Fun,
                AuthContext,
                Retries,
                <<"Invalid OTP code. Please try again:">>,
                Opts
            );
        {ok, {401, Headers, _Body}} = Response ->
            case detect_auth_error(Headers) of
                otp_required ->
                    handle_otp_retry(
                        Config, Fun, AuthContext, Retries, <<"Enter OTP code:">>, Opts
                    );
                invalid_totp ->
                    Msg =
                        case maps:get(otp_error, Retries) of
                            invalid_totp -> <<"Invalid OTP code. Please try again:">>;
                            _ -> <<"Enter OTP code:">>
                        end,
                    handle_otp_retry(Config, Fun, AuthContext, Retries, Msg, Opts);
                token_expired ->
                    handle_token_refresh_retry(Config, Fun, AuthContext, Retries, Response, Opts);
                none ->
                    Response
            end;
        Other ->
            Other
    end.

%% @private
handle_otp_retry(_Config, _Fun, _AuthContext, #{otp := OtpRetries}, _Message, _Opts) when
    OtpRetries >= ?MAX_OTP_RETRIES
->
    {error, {auth_error, otp_max_retries}};
handle_otp_retry(Config, Fun, AuthContext, #{otp := OtpRetries} = Retries, Message, Opts) ->
    case call_callback(Config, prompt_otp, [Message]) of
        {ok, OtpCode} ->
            NewConfig = Config#{api_otp => OtpCode},
            NewRetries = Retries#{otp := OtpRetries + 1, otp_error := invalid_totp},
            execute_with_retry(NewConfig, Fun, AuthContext, NewRetries, Opts);
        cancelled ->
            {error, {auth_error, otp_cancelled}}
    end.

%% @private
%% A 401 that says the token expired is answered by renewing the credential at
%% its source and running the request once more, the way a repository request
%% is. The renewal is counted, so a server that answers token_expired to every
%% bearer we send it gets a bounded number of requests rather than a loop.
handle_token_refresh_retry(
    _Config, _Fun, _AuthContext, #{token := TokenRetries}, Response, _Opts
) when
    TokenRetries >= ?MAX_TOKEN_RETRIES
->
    Response;
handle_token_refresh_retry(
    Config, Fun, AuthContext, #{token := TokenRetries} = Retries, _Response, Opts
) ->
    NewRetries = Retries#{token := TokenRetries + 1},
    %% Only attempt refresh if we have a refresh token
    case maps:get(has_refresh_token, AuthContext, false) of
        true ->
            case resolve_oauth_token_with_context(Config, true) of
                {ok, NewBearerToken, NewAuthContext} ->
                    NewConfig = Config#{api_key => NewBearerToken},
                    execute_with_retry(NewConfig, Fun, NewAuthContext, NewRetries, Opts);
                {error, _} ->
                    maybe_reauthenticate(Config, Fun, NewRetries, Opts)
            end;
        false ->
            maybe_reauthenticate(Config, Fun, NewRetries, Opts)
    end.

%% @private
%% After token refresh failure, prompt the user to re-authenticate via device auth
%% (only when auth_inline is true). Mirrors Hex.OAuth.reauthenticate/1.
maybe_reauthenticate(Config, Fun, Retries, Opts) ->
    AuthInline = proplists:get_value(auth_inline, Opts, true),
    case AuthInline of
        true ->
            maybe_authenticate_and_retry(api, Config, Fun, token_refresh_failed, Retries, Opts);
        false ->
            {error, {auth_error, token_refresh_failed}}
    end.

%% @private
-spec detect_auth_error(mix_hex_http:headers()) -> otp_required | invalid_totp | token_expired | none.
detect_auth_error(Headers) ->
    case maps:get(<<"www-authenticate">>, Headers, undefined) of
        undefined ->
            none;
        Value ->
            parse_www_authenticate(Value)
    end.

%% @private
parse_www_authenticate(Value) when is_binary(Value) ->
    case Value of
        <<"Bearer realm=\"hex\", error=\"totp_required\"", _/binary>> ->
            otp_required;
        <<"Bearer realm=\"hex\", error=\"invalid_totp\"", _/binary>> ->
            invalid_totp;
        <<"Bearer realm=\"hex\", error=\"token_expired\"", _/binary>> ->
            token_expired;
        _ ->
            none
    end.

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

%% @private
call_callback(Config, Name, Args) ->
    #{cli_auth_callbacks := Callbacks} = Config,
    Fun = maps:get(Name, Callbacks),
    erlang:apply(Fun, Args).

%% @private
%% Hands the build tool the organizations this session has to authenticate for.
%% A grant that flagged none carries the empty list, so a set that has been
%% resolved does not linger. A grant whose set could not be read is not
%% reported: the empty list would be taken for the server saying there is
%% nothing, and the build tool would drop the organizations it holds.
report_sso_reauth(Config, {ok, Organizations}) when is_list(Organizations) ->
    maybe_call_callback(Config, sso_reauth, [Organizations]);
report_sso_reauth(_Config, error) ->
    ok.

%% @private
%% Like call_callback/3 but for optional callbacks: returns ok without doing
%% anything when the callback is not provided.
maybe_call_callback(Config, Name, Args) ->
    #{cli_auth_callbacks := Callbacks} = Config,
    case maps:find(Name, Callbacks) of
        {ok, Fun} -> erlang:apply(Fun, Args);
        error -> ok
    end.
