%% Vendored from hex_core v0.21.0 (564a33e), do not edit manually

%% @doc
%% HTTP contract.
-module(mix_hex_http).
-export([request/5, request_to_file/6, bearer_error/1]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("mix_hex_core.hrl").

-type method() :: get | post | put | patch | delete.
-type status() :: non_neg_integer().
-export_type([status/0]).
-type headers() :: #{binary() => binary()}.
-export_type([headers/0]).
-type body() :: {ContentType :: binary(), Body :: binary()} | undefined.
-export_type([body/0]).
-type adapter_config() :: map().
-export_type([adapter_config/0]).

-callback request(method(), URI :: binary(), headers(), body(), adapter_config()) ->
    {ok, {status(), headers(), binary()}}
    | {error, term()}.

-callback request_to_file(
    method(), URI :: binary(), headers(), body(), file:name_all(), adapter_config()
) ->
    {ok, {status(), headers()}} | {error, term()}.

-spec request(mix_hex_core:config(), method(), URI :: binary(), headers(), body()) ->
    {ok, {status(), headers(), binary()}} | {error, term()}.
request(Config, Method, URI, Headers, Body) when is_binary(URI) and is_map(Headers) ->
    {Adapter, AdapterConfig} = adapter(Config),
    UserAgentFragment = maps:get(http_user_agent_fragment, Config),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:request(Method, URI, Headers2, Body, AdapterConfig).

-spec request_to_file(
    mix_hex_core:config(), method(), URI :: binary(), headers(), body(), file:name_all()
) ->
    {ok, {status(), headers()}} | {error, term()}.
request_to_file(Config, Method, URI, Headers, Body, Filename) when
    is_binary(URI) and is_map(Headers)
->
    {Adapter, AdapterConfig} = adapter(Config),
    UserAgentFragment = maps:get(http_user_agent_fragment, Config),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:request_to_file(Method, URI, Headers2, Body, Filename, AdapterConfig).

%% @doc
%% The `error' parameter of the first Bearer challenge that has one in a
%% response's `www-authenticate' header (RFC 6750 section 3), or `undefined'
%% when there is none.
%%
%% The header is read the way RFC 9110 section 11.6.1 defines it: it can hold
%% several challenges, a scheme can be followed by a token68 or by
%% parameters, scheme and parameter names are case-insensitive, and a
%% parameter value is a token or a quoted string.
-spec bearer_error(headers()) -> binary() | undefined.
bearer_error(Headers) ->
    case maps:get(<<"www-authenticate">>, Headers, undefined) of
        Value when is_binary(Value) ->
            case [Error || {<<"bearer">>, #{<<"error">> := Error}} <- parse_challenges(Value)] of
                [Error | _] -> Error;
                [] -> undefined
            end;
        undefined ->
            undefined
    end.

%% @private
user_agent(UserAgentFragment) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    iolist_to_binary(["hex_core/", ?HEX_CORE_VERSION, " ", UserAgentFragment, OTPString]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
adapter(Config) ->
    case maps:get(http_adapter, Config, {mix_hex_http_httpc, #{}}) of
        {Adapter, AdapterConfig} ->
            {Adapter, AdapterConfig};
        %% TODO: remove in v0.9
        Adapter when is_atom(Adapter) ->
            AdapterConfig = maps:get(http_adapter_config, Config, #{}),
            io:format(
                "[mix_hex_http] setting #{http_adapter => Module, http_adapter_config => Map} "
                "is deprecated in favour of #{http_adapter => {Module, Map}}~n"
            ),
            {Adapter, AdapterConfig}
    end.

%% @private
put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.

%%====================================================================
%% Internal functions - WWW-Authenticate
%%====================================================================

%% @private
%% WWW-Authenticate = #challenge
%% challenge = auth-scheme [ 1*SP ( token68 / #auth-param ) ]
%%
%% Parsing stops at the first thing that isn't part of a challenge and
%% returns the challenges read up to there.
parse_challenges(Value) ->
    parse_challenges(skip_separators(Value), []).

parse_challenges(Value, Acc) ->
    case take_token(Value) of
        {<<>>, _Rest} ->
            lists:reverse(Acc);
        {Scheme, Rest} ->
            {Params, Rest1} = parse_challenge_params(Rest),
            parse_challenges(skip_separators(Rest1), [{lowercase(Scheme), Params} | Acc])
    end.

%% A scheme is followed by a space and either a token68 or parameters, or
%% by nothing. A token68 ends the challenge, so it is followed by a comma or
%% the end of the field.
parse_challenge_params(<<C, _/binary>> = Value) when C =:= $\s ->
    Value1 = skip_ows(Value),
    case take_token68(Value1) of
        {<<_, _/binary>>, Rest} ->
            case skip_ows(Rest) of
                <<>> -> {#{}, <<>>};
                <<$,, _/binary>> = Rest1 -> {#{}, Rest1};
                _ -> parse_params(Value1, #{})
            end;
        {<<>>, _Rest} ->
            %% The parameter list can start with empty elements
            parse_params(skip_separators(Value1), #{})
    end;
parse_challenge_params(Value) ->
    {#{}, Value}.

%% auth-param = token BWS "=" BWS ( token / quoted-string )
%%
%% A token that isn't followed by "=" is the scheme of the next challenge.
parse_params(Value, Acc) ->
    case take_token(Value) of
        {<<>>, _Rest} ->
            {Acc, Value};
        {Name, Rest} ->
            case skip_ows(Rest) of
                <<$=, Rest1/binary>> ->
                    case take_param_value(skip_ows(Rest1)) of
                        {ok, ParamValue, Rest2} ->
                            Acc1 = put_new(lowercase(Name), ParamValue, Acc),
                            next_param(skip_ows(Rest2), Acc1);
                        error ->
                            {Acc, <<>>}
                    end;
                _ ->
                    {Acc, Value}
            end
    end.

next_param(<<$,, Rest/binary>>, Acc) ->
    parse_params(skip_separators(Rest), Acc);
next_param(Value, Acc) ->
    {Acc, Value}.

take_param_value(<<$", Rest/binary>>) ->
    take_quoted_string(Rest, <<>>);
take_param_value(Value) ->
    case take_token(Value) of
        {<<>>, _Rest} -> error;
        {Token, Rest} -> {ok, Token, Rest}
    end.

%% quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
take_quoted_string(<<$", Rest/binary>>, Acc) ->
    {ok, Acc, Rest};
take_quoted_string(<<$\\, C, Rest/binary>>, Acc) ->
    take_quoted_string(Rest, <<Acc/binary, C>>);
take_quoted_string(<<C, Rest/binary>>, Acc) when C =/= $\\ ->
    take_quoted_string(Rest, <<Acc/binary, C>>);
take_quoted_string(_Value, _Acc) ->
    error.

take_token(Value) ->
    take_while(fun is_tchar/1, Value).

%% token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" ) *"="
take_token68(Value) ->
    case take_while(fun is_token68_char/1, Value) of
        {<<>>, _Rest} = Empty ->
            Empty;
        {Token, Rest} ->
            {Padding, Rest1} = take_while(fun(C) -> C =:= $= end, Rest),
            {<<Token/binary, Padding/binary>>, Rest1}
    end.

take_while(Pred, Value) ->
    take_while(Pred, Value, 0).

take_while(Pred, Value, N) ->
    case Value of
        <<_:N/binary, C, _/binary>> ->
            case Pred(C) of
                true -> take_while(Pred, Value, N + 1);
                false -> split_binary(Value, N)
            end;
        _ ->
            split_binary(Value, N)
    end.

%% Commas and whitespace between the elements of a list, which can also be
%% empty (RFC 9110 section 5.6.1).
skip_separators(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $, ->
    skip_separators(Rest);
skip_separators(Value) ->
    Value.

skip_ows(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t ->
    skip_ows(Rest);
skip_ows(Value) ->
    Value.

is_tchar(C) when C >= $a, C =< $z -> true;
is_tchar(C) when C >= $A, C =< $Z -> true;
is_tchar(C) when C >= $0, C =< $9 -> true;
is_tchar(C) -> lists:member(C, "!#$%&'*+-.^_`|~").

is_token68_char(C) when C >= $a, C =< $z -> true;
is_token68_char(C) when C >= $A, C =< $Z -> true;
is_token68_char(C) when C >= $0, C =< $9 -> true;
is_token68_char(C) -> lists:member(C, "-._~+/").

%% Scheme and parameter names are tokens, which are ASCII.
lowercase(Value) ->
    <<<<(ascii_lowercase(C))>> || <<C>> <= Value>>.

ascii_lowercase(C) when C >= $A, C =< $Z -> C + ($a - $A);
ascii_lowercase(C) -> C.
