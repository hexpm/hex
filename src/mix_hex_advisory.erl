%% Vendored from hex_core v0.17.0 (bcbaeaa), do not edit manually

%% @doc
%% Display-time deduplication of security advisories.
%%
%% Multiple advisory sources (EEF, GHSA, NVD, ...) can publish the same
%% vulnerability under different identifiers and cross-reference each other
%% via the `aliases' field. `group_for_display/1' groups such advisories,
%% picks a deterministic primary, and merges references and timestamps so
%% callers can render one entry per vulnerability.
-module(mix_hex_advisory).
-export([group_for_display/1]).

-type advisory() :: map().
-type group() :: map().

-spec group_for_display([advisory()]) -> [group()].
group_for_display(Advisories) ->
    %% Preserve input order of first member per group.
    {GroupOrder, GroupsByKey} =
        lists:foldl(
            fun(Adv, {Order, Map}) ->
                Key = group_key(Adv),
                case maps:is_key(Key, Map) of
                    true ->
                        Existing = maps:get(Key, Map),
                        {Order, Map#{Key := Existing ++ [Adv]}};
                    false ->
                        {Order ++ [Key], Map#{Key => [Adv]}}
                end
            end,
            {[], #{}},
            Advisories
        ),
    [merge_group(maps:get(Key, GroupsByKey)) || Key <- GroupOrder].

%%====================================================================
%% Grouping
%%====================================================================

%% Key is the first CVE-prefixed identifier across {id, aliases}, else id.
group_key(#{id := Id} = Adv) ->
    Ids = [Id | maps:get(aliases, Adv, [])],
    case lists:dropwhile(fun(I) -> not is_cve(I) end, Ids) of
        [Cve | _] -> Cve;
        [] -> Id
    end.

is_cve(<<"CVE-", _/binary>>) -> true;
is_cve(_) -> false.

%%====================================================================
%% Merging
%%====================================================================

merge_group(Advisories) ->
    Primary = pick_primary(Advisories),
    Rest = [A || A <- Advisories, maps:get(id, A) =/= maps:get(id, Primary)],
    Ordered = [Primary | Rest],
    Primary#{
        aliases => display_aliases(Primary, Ordered),
        published_at => min_timestamp(Ordered),
        modified_at => max_timestamp(Ordered),
        references => merge_references(Ordered)
    }.

pick_primary(Advisories) ->
    [Primary | _] = lists:sort(
        fun(A, B) -> source_key(A) =< source_key(B) end,
        Advisories
    ),
    Primary.

source_key(#{id := Id}) -> {source_priority(Id), Id}.

source_priority(<<"EEF-", _/binary>>) -> 0;
source_priority(<<"GHSA-", _/binary>>) -> 1;
source_priority(<<"NVD-", _/binary>>) -> 2;
source_priority(_) -> 3.

%%====================================================================
%% Aliases
%%====================================================================

display_aliases(Primary, Advisories) ->
    PrimaryId = maps:get(id, Primary),
    AdvisoryIds = sets:from_list([maps:get(id, A) || A <- Advisories]),
    AllIds = lists:flatmap(fun identifiers/1, Advisories),
    Unique = uniq(AllIds),
    [
        #{
            id => Id,
            url => alias_url(Id, AdvisoryIds)
        }
     || Id <- Unique, Id =/= PrimaryId
    ].

identifiers(Advisory) ->
    [maps:get(id, Advisory) | maps:get(aliases, Advisory, [])].

alias_url(Id, AdvisoryIds) ->
    case sets:is_element(Id, AdvisoryIds) of
        true ->
            Encoded = uri_string:quote(Id),
            <<"https://osv.dev/vulnerability/", Encoded/binary>>;
        false ->
            undefined
    end.

%%====================================================================
%% Timestamps
%%====================================================================

min_timestamp(Advisories) ->
    aggregate_timestamp(Advisories, published_at, fun erlang:'<'/2).

max_timestamp(Advisories) ->
    aggregate_timestamp(Advisories, modified_at, fun erlang:'>'/2).

aggregate_timestamp(Advisories, Field, Cmp) ->
    Stamps = [T || A <- Advisories, (T = maps:get(Field, A, undefined)) =/= undefined],
    case Stamps of
        [] ->
            undefined;
        [First | Rest] ->
            lists:foldl(
                fun(T, Acc) ->
                    case Cmp(ts_tuple(T), ts_tuple(Acc)) of
                        true -> T;
                        false -> Acc
                    end
                end,
                First,
                Rest
            )
    end.

ts_tuple(#{seconds := S, nanos := N}) -> {S, N};
ts_tuple(#{seconds := S}) -> {S, 0}.

%%====================================================================
%% References
%%====================================================================

merge_references(Advisories) ->
    AllRefs = lists:flatmap(fun(A) -> maps:get(references, A, []) end, Advisories),
    {UrlOrder, ByUrl} =
        lists:foldl(
            fun(#{url := Url, type := Type}, {Order, Map}) ->
                case maps:is_key(Url, Map) of
                    true ->
                        Existing = maps:get(Url, Map),
                        Map2 = Map#{Url := uniq(Existing ++ [Type])},
                        {Order, Map2};
                    false ->
                        {Order ++ [Url], Map#{Url => [Type]}}
                end
            end,
            {[], #{}},
            AllRefs
        ),
    [#{url => Url, types => maps:get(Url, ByUrl)} || Url <- UrlOrder].

%%====================================================================
%% Misc
%%====================================================================

uniq(List) ->
    {_, Out} =
        lists:foldl(
            fun(X, {Seen, Acc}) ->
                case sets:is_element(X, Seen) of
                    true -> {Seen, Acc};
                    false -> {sets:add_element(X, Seen), Acc ++ [X]}
                end
            end,
            {sets:new(), []},
            List
        ),
    Out.
