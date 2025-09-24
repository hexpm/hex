%% Vendored from hex_core v0.11.0 (a1bf7f7), do not edit manually

%% @doc
%% Hex HTTP API - Packages.
-module(mix_hex_api_package).
-export([get/2, search/3]).

%% @doc
%% Gets a package.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_package:get(mix_hex_core:default_config(), <<"package">>).
%% {ok, {200, ..., #{
%%     <<"name">> => <<"package1">>,
%%     <<"meta">> => #{
%%         <<"description">> => ...,
%%         <<"licenses">> => ...,
%%         <<"links">> => ...,
%%         <<"maintainers">> => ...
%%     },
%%     ...,
%%     <<"releases">> => [
%%         #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%         #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%         ...
%%     ]}}}
%% '''
%% @end
-spec get(mix_hex_core:config(), binary()) -> mix_hex_api:response().
get(Config, Name) when is_map(Config) and is_binary(Name) ->
    Path = mix_hex_api:build_repository_path(Config, ["packages", Name]),
    mix_hex_api:get(Config, Path).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%% > mix_hex_api_package:search(mix_hex_core:default_config(), <<"package">>, [{page, 1}]).
%% {ok, {200, ..., [
%%     #{<<"name">> => <<"package1">>, ...},
%%     ...
%% ]}}
%% '''
-spec search(mix_hex_core:config(), binary(), [{term(), term()}]) -> mix_hex_api:response().
search(Config, Query, SearchParams) when
    is_map(Config) and is_binary(Query) and is_list(SearchParams)
->
    QueryString = mix_hex_api:encode_query_string([{search, Query} | SearchParams]),
    Path = mix_hex_api:join_path_segments(mix_hex_api:build_repository_path(Config, ["packages"])),
    PathQuery = <<Path/binary, "?", QueryString/binary>>,
    mix_hex_api:get(Config, PathQuery).
