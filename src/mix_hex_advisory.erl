%% Vendored from hex_core v0.18.0 (0f47da2), do not edit manually

%% @doc
%% Display-time deduplication of security advisories.
%%
%% Multiple advisory sources (EEF, GHSA, NVD, ...) can publish the same
%% vulnerability under different identifiers and cross-reference each other
%% via the `aliases' field. `group_for_display/1' groups such advisories
%% and picks a deterministic primary so callers can render one entry per
%% vulnerability.
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
    Primary#{aliases => display_aliases(Primary, [Primary | Rest])}.

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
        true -> <<"https://osv.dev/vulnerability/", Id/binary>>;
        false -> undefined
    end.

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
