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

-export([
    parse_uri_flight_id/1,
    parse_uri_flight_date_time/1,
    response/2,
    response/1,
    content/2,
    status/1
    ]).

-export_type([]).

-define(URI_FLIGHT_ID_FORMAT, "flight-~s").
-define(URI_FLIGHT_DATE_TIME_FORMAT, "~4d-~2d-~2dT~2d~2dZ").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Extracts the FlightId from the uri string parameter.
%% Format defined in URI_FLIGHT_ID_FORMAT.
%%
%% @end
%%------------------------------------------------------------------------------

-spec parse_uri_flight_id(list()) -> list().

parse_uri_flight_id(Uri_FlightId) ->
    {ok, [FlightId], _} =
        io_lib:fread(?URI_FLIGHT_ID_FORMAT, Uri_FlightId),
    FlightId.

%%------------------------------------------------------------------------------
%% @doc Extracts the FlightDateTime from the uri string parameter.
%% Format defined in URI_FLIGHT_DATE_TIME_FORMAT.
%%
%% @end
%%------------------------------------------------------------------------------

-spec parse_uri_flight_date_time(list()) -> {calendar:date(), calendar:time()}.

parse_uri_flight_date_time(Uri_FlightDateTime) ->
    {ok, [Year, Month, Day, Hour, Minute], _} =
        io_lib:fread(?URI_FLIGHT_DATE_TIME_FORMAT, Uri_FlightDateTime),
    {{Year, Month, Day}, {Hour, Minute, 0}}.

%%------------------------------------------------------------------------------
%% @doc Yaws response helper.
%%
%% @end
%%------------------------------------------------------------------------------

-spec response({content, term(), term()}, {status, term()}) -> [term()].

response(Content, Status) ->
    [Content, Status].

%%------------------------------------------------------------------------------
%% @doc Yaws response helper.
%%
%% @end
%%------------------------------------------------------------------------------

-spec response({status, term()}) -> [term()].

response(Status) ->
    [Status].

-spec content(binary(), binary()) -> {content, binary(), binary()}.

%%------------------------------------------------------------------------------
%% @doc Yaws response content helper.
%%
%% @end
%%------------------------------------------------------------------------------

content(ContentType, ContentBody) when is_binary(ContentType)
    andalso is_binary(ContentBody)  ->
        {content, ContentType, ContentBody}.

%%------------------------------------------------------------------------------
%% @doc Yaws response status helper.
%%
%% @end
%%------------------------------------------------------------------------------

-spec status(integer()) -> {status, integer()}.

status(StatusCode) when is_integer(StatusCode) ->
    {status, StatusCode}.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    % some setup here if needed
    % ok = ok,

    [
        {"parsing",
            [
                ?_assertMatch("111", parse_uri_flight_id("flight-111")),
                ?_assertMatch("flight-111", parse_uri_flight_id("flight-flight-111")),
                ?_assertException(error, {badmatch, _}, parse_uri_flight_id("something-111")),
                ?_assertMatch({{2013, 8, 29}, {12, 15, 0}}, parse_uri_flight_date_time("2013-08-29T1215Z")),
                ?_assertException(error, {badmatch, _}, parse_uri_flight_date_time("2013-08-29T1215"))
            ]
        }
    ].

-endif.
