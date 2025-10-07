%% Vendored from hex_core v0.11.0 (5cb699e), do not edit manually

%% @doc
%% Hex HTTP API - Authentication.
-module(mix_hex_api_auth).
-export([test/2]).

%% @doc
%% Test an auth key against a given domain and resource.
%%
%% Examples:
%%
%% ```
%% 1> Params = #{domain => <<"repository">>, resource => <<"gustafson_motors">>}.
%% 2> mix_hex_api_auth:test_key(mix_hex_core:default_config(), Params).
%% {ok,{204, ..., nil}}
%% '''
%% @end
-spec test(mix_hex_core:config(), map()) -> mix_hex_api:response().
test(Config, #{domain := Domain, resource := Resource}) ->
    URI = ["auth", "?domain=", Domain, "&resource=", Resource],
    mix_hex_api:get(Config, list_to_binary(URI)).
