%% Vendored from hex_core v0.11.0 (94a912d), do not edit manually

%% @doc
%% Hex HTTP API - Organization Members.
-module(mix_hex_api_organization_member).
-export([
    add/3,
    delete/2,
    get/2,
    list/1,
    update/3
]).

-type role() :: admin | write | read.

%% @doc
%% Lists the organization's members.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization_member:list(mix_hex_core:default_config(), #{api_organization => <<"acme">>}).
%% {ok, {200, ..., [#{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"admin">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }]}}
%% '''
%% @end
-spec list(mix_hex_core:config()) -> mix_hex_api:response().
list(Config) when is_map(Config) ->
    Path = mix_hex_api:build_organization_path(Config, ["members"]),
    mix_hex_api:get(Config, Path).

%% @doc
%% Gets an organization member.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization_member:get(mix_hex_core:default_config(), #{api_organization => <<"acme">>}, <<"user">>).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"admin">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec get(mix_hex_core:config(), binary()) -> mix_hex_api:response().
get(Config, UsernameOrEmail) when is_map(Config) and is_binary(UsernameOrEmail) ->
    Path = mix_hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    mix_hex_api:get(Config, Path).

%% @doc
%% Adds an organization member.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization_member:add(mix_hex_core:default_config(), #{api_organization => <<"acme">>}, <<"user">>, write).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"write">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec add(mix_hex_core:config(), binary(), role()) -> mix_hex_api:response().
add(Config, UsernameOrEmail, Role) when
    is_map(Config) and is_binary(UsernameOrEmail) and is_atom(Role)
->
    Path = mix_hex_api:build_organization_path(Config, ["members"]),
    Params = #{<<"name">> => UsernameOrEmail, <<"role">> => Role},
    mix_hex_api:post(Config, Path, Params).

%% @doc
%% Updates an organization member's role.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization_member:update(mix_hex_core:default_config(), #{api_organization => <<"acme">>}, <<"user">>, read).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"role">> => <<"read">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec update(mix_hex_core:config(), binary(), role()) -> mix_hex_api:response().
update(Config, UsernameOrEmail, Role) when
    is_map(Config) and is_binary(UsernameOrEmail) and is_atom(Role)
->
    Path = mix_hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    Params = #{<<"role">> => Role},
    mix_hex_api:post(Config, Path, Params).

%% @doc
%% Deletes an organization member.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_organization_member:delete(mix_hex_core:default_config(), #{api_organization => <<"acme">>}, <<"user">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec delete(mix_hex_core:config(), binary()) -> mix_hex_api:response().
delete(Config, UsernameOrEmail) when is_map(Config) and is_binary(UsernameOrEmail) ->
    Path = mix_hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    mix_hex_api:delete(Config, Path).
