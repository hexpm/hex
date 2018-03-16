%% Vendored from hex_erl, do not edit manually

-module(vendored_hex_registry).
-export([
    encode_names/1,
    decode_names/1,
    encode_versions/1,
    decode_versions/1,
    encode_package/1,
    decode_package/1,
    sign_protobuf/2,
    decode_signed/1,
    decode_and_verify_signed/2
]).
-include_lib("public_key/include/public_key.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Encode Names message.
encode_names(Names) ->
    hex_pb_names:encode_msg(Names, 'Names').

%% @doc
%% Decode message created with encode_names/1.
decode_names(Payload) ->
    hex_pb_names:decode_msg(Payload, 'Names').

%% @doc
%% Encode Versions message.
encode_versions(Versions) ->
    hex_pb_versions:encode_msg(Versions, 'Versions').

%% @doc
%% Decode message created with encode_versions/1.
decode_versions(Payload) ->
    hex_pb_versions:decode_msg(Payload, 'Versions').

%% @doc
%% Encode Package message.
encode_package(Package) ->
    vendored_hex_pb_package:encode_msg(Package, 'Package').

%% @doc
%% Decode message created with encode_package/1.
decode_package(Payload) ->
    vendored_hex_pb_package:decode_msg(Payload, 'Package').

%% @doc
%% Encode Signed message.
sign_protobuf(Payload, PrivateKey) ->
    Signature = sign(Payload, PrivateKey),
    vendored_hex_pb_signed:encode_msg(#{payload => Payload, signature => Signature}, 'Signed').

%% @doc
%% Decode message created with sign_protobuf/2 without verification.
decode_signed(Signed) ->
    vendored_hex_pb_signed:decode_msg(Signed, 'Signed').

%% @doc
%% Decode message created with sign_protobuf/2 and verify it with PublicKey.
decode_and_verify_signed(Signed, PublicKey) ->
    #{payload := Payload, signature := Signature} = decode_signed(Signed),
    verify(Payload, Signature, PublicKey).

%%====================================================================
%% Internal functions
%%====================================================================

sign(Binary, PrivateKeyBinary) ->
    {ok, PrivateKey} = key(PrivateKeyBinary),
    public_key:sign(Binary, sha512, PrivateKey).

verify(Binary, Signature, PublicKeyBinary) ->
    case key(PublicKeyBinary) of
        {ok, PublicKey} ->
            case public_key:verify(Binary, sha512, Signature, PublicKey) of
                true -> {ok, Binary};
                false -> {error, unverified}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

key(#'RSAPublicKey'{} = Key) ->
    {ok, Key};
key(#'RSAPrivateKey'{} = Key) ->
    {ok, Key};
key(Binary) when is_binary(Binary) ->
    case public_key:pem_decode(Binary) of
        [Entry | _] -> {ok, public_key:pem_entry_decode(Entry)};
        _ -> {error, bad_key}
    end.
