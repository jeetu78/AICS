%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest utility functions
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_utils).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([json_encode/1,
         json_decode/1,
         parse_uri_id/1,
         record_to_json_value/1,
         resource_context_uri/1,
         resource_context_uri/2,
         error_view/3]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include("ea_aics_rest.hrl").

%% ===================================================================
%%  API
%% ===================================================================

-spec json_encode(term()) -> binary().

json_encode(Term) when is_list(Term) ->
    jsx:encode(Term).

-spec json_decode(binary()) -> term().

json_decode(Binary) when is_binary(Binary) ->
    jsx:decode(Binary).

-spec parse_uri_id(list()) -> binary().

parse_uri_id(Uri_Id) ->
    list_to_binary(Uri_Id).

-spec record_to_json_value(term()) -> term().

record_to_json_value(undefined) ->
    null;
record_to_json_value({datetime, Datetime}) ->
    iso8601:format(Datetime);
record_to_json_value(Value) ->
    Value.

-spec resource_context_uri(#arg{}) -> binary().

resource_context_uri(WebArg) ->
    resource_context_uri(WebArg, <<"">>).

-spec resource_context_uri(#arg{}, binary()) -> binary().

resource_context_uri(WebArg, Context) ->
    WebRedirSelf = yaws_api:redirect_self(WebArg),
    Scheme = list_to_binary(WebRedirSelf#redir_self.scheme_str),
    Host = list_to_binary(WebRedirSelf#redir_self.host),
    Port = list_to_binary(WebRedirSelf#redir_self.port_str),
    Separator = <<"/">>,
    <<Scheme/binary, Host/binary, Port/binary, Separator/binary, Context/binary>>.

-spec error_view(#arg{}, [string()], atom()) -> list().

error_view(_WebArg, _Path, ErrorReason) when is_atom(ErrorReason) ->
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    JsonView = [{<<"errors">>, [[{<<"message">>, atom_to_binary(ErrorReason, utf8)},
                                 {<<"code">>, 0}]]}],
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus].

%% ===================================================================
%%  Internal Functions
%% ===================================================================

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    [].

-endif.
