%% Vendored from hex_core v0.11.0 (8d1b5a0), do not edit manually

%% @doc
%% Hex HTTP API - Organizations.
-module(mix_hex_api_organization).
-export([
    get/1,
    list/1,
    update/2
]).

%% @doc
%% Lists all organizations you are member of.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization:list(mix_hex_core:default_config(), #{api_organization => <<"acme">>}).
%% {ok, {200, ..., [#{
%%      <<"billing_active">> => true,
%%      <<"inserted_at">> => <<"2017-08-22T22:19:53Z">>,
%%      <<"name">> => <<"acme">>,
%%      <<"updated_at">> => <<"2019-01-18T08:47:29Z">>}
%%      }]}}
%% '''
-spec list(mix_hex_core:config()) -> mix_hex_api:response().
list(Config) when is_map(Config) ->
    mix_hex_api:get(Config, ["orgs"]).

%% @doc
%% Gets an organzation.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization:get(mix_hex_core:default_config(), #{api_organization => <<"acme">>}).
%% {ok, {200, ..., #{
%%      <<"billing_active">> => true,
%%      <<"inserted_at">> => <<"2017-08-22T22:19:53Z">>,
%%      <<"name">> => <<"acme">>,
%%      <<"seats">> => 42,
%%      <<"updated_at">> => <<"2019-01-18T08:47:29Z">>}
%%      }}}
%% '''
-spec get(mix_hex_core:config()) -> mix_hex_api:response().
get(Config) when is_map(Config) ->
    Path = mix_hex_api:build_organization_path(Config, []),
    mix_hex_api:get(Config, Path).

%% @doc
%% Updates the number of seats in an organzation.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization:get(mix_hex_core:default_config(), #{api_organization => <<"acme">>}, 42).
%% {ok, {200, ..., #{
%%      <<"billing_active">> => true,
%%      <<"inserted_at">> => <<"2017-08-22T22:19:53Z">>,
%%      <<"name">> => <<"acme">>,
%%      <<"seats">> => 42,
%%      <<"updated_at">> => <<"2019-01-18T08:47:29Z">>}
%%      }}}
%% '''
-spec update(mix_hex_core:config(), pos_integer()) -> mix_hex_api:response().
update(Config, Seats) when is_map(Config) and is_integer(Seats) ->
    Path = mix_hex_api:build_organization_path(Config, []),
    Params = #{<<"seats">> => Seats},
    mix_hex_api:post(Config, Path, Params).
