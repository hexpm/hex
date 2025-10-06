%% Vendored from hex_core v0.11.0 (5cb699e), do not edit manually

%% @doc
%% Hex HTTP API - Short URLs.
-module(mix_hex_api_short_url).
-export([create/2]).

%% @doc
%% Creates a short URL.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_short_url:create(mix_hex_core:default_config(), <<"https://hex.pm/packages/example">>).
%% {ok, {201, ..., #{<<"url">> => <<"https://hex.pm/l/XXXXX">>}}}
%% '''
%% @end
-spec create(mix_hex_core:config(), binary()) -> mix_hex_api:response().
create(Config, URL) when is_map(Config) and is_binary(URL) ->
    Body = #{<<"url">> => URL},
    mix_hex_api:post(Config, ["short_url"], Body).
