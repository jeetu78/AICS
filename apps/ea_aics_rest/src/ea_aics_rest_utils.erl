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
         record_to_json_value/1]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").

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
