%% Vendored from hex_core v0.11.0 (8d1b5a0), do not edit manually

%% @doc
%% Hex HTTP API - OAuth.
-module(mix_hex_api_oauth).
-export([
    device_authorization/2,
    token/2,
    revoke/2
]).

%% @doc
%% Initiates the OAuth device authorization flow.
%%
%% Returns device code, user code, and verification URIs for user authentication.
%%
%% Examples:
%%
%% ```
%% 1> Config = mix_hex_core:default_config().
%% 2> Params = #{client_id => <<"cli">>, scope => <<"api:write">>}.
%% 3> mix_hex_api_oauth:device_authorization(Config, Params).
%% {ok,{200, ..., #{
%%     <<"device_code">> => <<"...">>,
%%     <<"user_code">> => <<"ABCD-1234">>,
%%     <<"verification_uri">> => <<"https://hex.pm/oauth/device">>,
%%     <<"verification_uri_complete">> => <<"https://hex.pm/oauth/device?user_code=ABCD-1234">>,
%%     <<"expires_in">> => 600,
%%     <<"interval">> => 5
%% }}}
%% '''
%% @end
-spec device_authorization(mix_hex_core:config(), map()) -> mix_hex_api:response().
device_authorization(Config, Params) ->
    Path = <<"oauth/device_authorization">>,
    mix_hex_api:post(Config, Path, Params).

%% @doc
%% OAuth token endpoint supporting multiple grant types.
%%
%% Supported grant types:
%% - `authorization_code` - Exchange authorization code for token
%% - `urn:ietf:params:oauth:grant-type:device_code` - Poll for device authorization
%% - `refresh_token` - Refresh an existing token
%% - `urn:ietf:params:oauth:grant-type:token-exchange` - Token exchange (RFC 8693)
%%
%% ## Device Code Grant
%%
%% ```
%% 1> Config = mix_hex_core:default_config().
%% 2> Params = #{
%%     grant_type => <<"urn:ietf:params:oauth:grant-type:device_code">>,
%%     device_code => <<"...">>,
%%     client_id => <<"cli">>
%% }.
%% 3> mix_hex_api_oauth:token(Config, Params).
%% '''
%%
%% Returns:
%% - `{ok, {200, _, Token}}` - Authorization complete
%% - `{ok, {400, _, #{<<"error">> => <<"authorization_pending">>}}}` - Still waiting
%% - `{ok, {400, _, #{<<"error">> => <<"slow_down">>}}}` - Polling too fast
%% - `{ok, {400, _, #{<<"error">> => <<"expired_token">>}}}` - Code expired
%% - `{ok, {403, _, #{<<"error">> => <<"access_denied">>}}}` - User denied
%%
%% ## Authorization Code Grant
%%
%% ```
%% 1> Config = mix_hex_core:default_config().
%% 2> Params = #{
%%     grant_type => <<"authorization_code">>,
%%     code => <<"...">>,
%%     client_id => <<"...">>,
%%     client_secret => <<"...">>,
%%     redirect_uri => <<"...">>,
%%     code_verifier => <<"...">>
%% }.
%% 3> mix_hex_api_oauth:token(Config, Params).
%% '''
%%
%% ## Refresh Token Grant
%%
%% ```
%% 1> Config = mix_hex_core:default_config().
%% 2> Params = #{
%%     grant_type => <<"refresh_token">>,
%%     refresh_token => <<"...">>,
%%     client_id => <<"...">>,
%%     client_secret => <<"...">>
%% }.
%% 3> mix_hex_api_oauth:token(Config, Params).
%% '''
%%
%% ## Token Exchange Grant (RFC 8693)
%%
%% ```
%% 1> Config = mix_hex_core:default_config().
%% 2> Params = #{
%%     grant_type => <<"urn:ietf:params:oauth:grant-type:token-exchange">>,
%%     subject_token => <<"...">>,
%%     subject_token_type => <<"urn:x-oath:params:oauth:token-type:key">>,
%%     client_id => <<"...">>,
%%     scope => <<"api:read">>
%% }.
%% 3> mix_hex_api_oauth:token(Config, Params).
%% '''
%%
%% Successful response includes:
%% ```
%% #{
%%     <<"access_token">> => <<"...">>,
%%     <<"refresh_token">> => <<"...">>,
%%     <<"token_type">> => <<"Bearer">>,
%%     <<"expires_in">> => 3600
%% }
%% '''
%% @end
-spec token(mix_hex_core:config(), map()) -> mix_hex_api:response().
token(Config, Params) ->
    Path = <<"oauth/token">>,
    mix_hex_api:post(Config, Path, Params).

%% @doc
%% Revokes an OAuth token (RFC 7009).
%%
%% Can revoke either access tokens or refresh tokens.
%% Returns 200 OK regardless of whether the token was found,
%% following RFC 7009 security recommendations.
%%
%% Examples:
%%
%% ```
%% 1> Config = mix_hex_core:default_config().
%% 2> Params = #{
%%     token => <<"...">>,
%%     client_id => <<"cli">>,
%%     token_type_hint => <<"access_token">>  % optional
%% }.
%% 3> mix_hex_api_oauth:revoke(Config, Params).
%% {ok, {200, ..., nil}}
%% '''
%% @end
-spec revoke(mix_hex_core:config(), map()) -> mix_hex_api:response().
revoke(Config, Params) ->
    Path = <<"oauth/revoke">>,
    mix_hex_api:post(Config, Path, Params).