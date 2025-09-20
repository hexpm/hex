%% Vendored from hex_core v0.11.0 (8d1b5a0), do not edit manually

%% @doc
%% Hex HTTP API - Package Owners.
-module(mix_hex_api_package_owner).
-export([
    add/5,
    delete/3,
    get/3,
    list/2
]).

%% @doc
%% Lists the packages owners.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_package_owner:list(mix_hex_core:default_config(), <<"package">>).
%% {ok, {200, ..., [#{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"full_name">> => <<"John Doe">>,
%%      <<"handles">> => #{...},
%%      <<"inserted_at">> => <<"2014-04-21T17:20:12Z">>,
%%      <<"level">> => <<"full">>,
%%      <<"updated_at">> => <<"2019-08-04T19:28:05Z">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }]}}
%% '''
%% @end
-spec list(mix_hex_core:config(), binary()) -> mix_hex_api:response().
list(Config, PackageName) when is_binary(PackageName) ->
    Path = mix_hex_api:build_repository_path(Config, ["packages", PackageName, "owners"]),
    mix_hex_api:get(Config, Path).

%% @doc
%% Gets a packages owner.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_package_owner:get(mix_hex_core:default_config(), <<"package">>, <<"user">>).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"full_name">> => <<"John Doe">>,
%%      <<"handles">> => #{...},
%%      <<"inserted_at">> => <<"2014-04-21T17:20:12Z">>,
%%      <<"level">> => <<"full">>,
%%      <<"updated_at">> => <<"2019-08-04T19:28:05Z">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec get(mix_hex_core:config(), binary(), binary()) -> mix_hex_api:response().
get(Config, PackageName, UsernameOrEmail) when
    is_map(Config) and is_binary(PackageName) and is_binary(UsernameOrEmail)
->
    Path = mix_hex_api:build_repository_path(Config, [
        "packages", PackageName, "owners", UsernameOrEmail
    ]),
    mix_hex_api:get(Config, Path).

%% @doc
%% Adds a packages owner.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_package_owner:add(mix_hex_core:default_config(), <<"package">>, <<"user">>, <<"full">>, false).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"full_name">> => <<"John Doe">>,
%%      <<"handles">> => #{...},
%%      <<"inserted_at">> => <<"2014-04-21T17:20:12Z">>,
%%      <<"level">> => <<"full">>,
%%      <<"updated_at">> => <<"2019-08-04T19:28:05Z">>,
%%      <<"url">> => <<"https://hex.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec add(mix_hex_core:config(), binary(), binary(), binary(), boolean()) -> mix_hex_api:response().
add(Config, PackageName, UsernameOrEmail, Level, Transfer) when
    is_binary(PackageName) and is_binary(UsernameOrEmail) and is_map(Config) and is_binary(Level) and
        is_boolean(Transfer)
->
    Path = mix_hex_api:build_repository_path(Config, [
        "packages", PackageName, "owners", UsernameOrEmail
    ]),
    mix_hex_api:put(Config, Path, #{<<"level">> => Level, <<"transfer">> => Transfer}).

%% @doc
%% Deletes a packages owner.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_package_owner:delete(mix_hex_core:default_config(), <<"package">>, <<"user">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec delete(mix_hex_core:config(), binary(), binary()) -> mix_hex_api:response().
delete(Config, PackageName, UsernameOrEmail) when
    is_map(Config) and is_binary(PackageName) and is_binary(UsernameOrEmail)
->
    Path = mix_hex_api:build_repository_path(Config, [
        "packages", PackageName, "owners", UsernameOrEmail
    ]),
    mix_hex_api:delete(Config, Path).
