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
    parse_uri_ancillary_id/1,
    parse_uri_ancillary_booking_id/1
    ]).

-export_type([]).

-define(URI_FLIGHT_ID_FORMAT, "flight-~s").
-define(URI_FLIGHT_DATE_TIME_FORMAT, "~4d-~2d-~2dT~2d~2dZ").
-define(URI_ANCILLARY_ID_FORMAT, "ancillary-~s").
-define(URI_ANCILLARY_BOOKING_ID_FORMAT, "ancillary-booking-~s").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Extracts the FlightId from the uri string parameter.
%% Format defined in URI_FLIGHT_ID_FORMAT.
%%
%% @end
%%------------------------------------------------------------------------------

-spec parse_uri_flight_id(string()) -> string().

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

-spec parse_uri_flight_date_time(string()) -> {calendar:date(), calendar:time()}.

parse_uri_flight_date_time(Uri_FlightDateTime) ->
    {ok, [Year, Month, Day, Hour, Minute], _} =
        io_lib:fread(?URI_FLIGHT_DATE_TIME_FORMAT, Uri_FlightDateTime),
    {{Year, Month, Day}, {Hour, Minute, 0}}.

%%------------------------------------------------------------------------------
%% @doc Extracts the AncillaryId from the uri string parameter.
%% Format defined in URI_ANCILLARY_ID_FORMAT.
%%
%% @end
%%------------------------------------------------------------------------------

-spec parse_uri_ancillary_id(string()) -> string().

parse_uri_ancillary_id(Uri_AncillaryId) ->
    {ok, [AncillaryId], _} =
        io_lib:fread(?URI_ANCILLARY_ID_FORMAT, Uri_AncillaryId),
    AncillaryId.

%%------------------------------------------------------------------------------
%% @doc Extracts the AncillaryBookingId from the uri string parameter.
%% Format defined in URI_ANCILLARY_BOOKING_ID_FORMAT.
%%
%% @end
%%------------------------------------------------------------------------------

-spec parse_uri_ancillary_booking_id(string()) -> string().

parse_uri_ancillary_booking_id(Uri_AncillaryBookingId) ->
    {ok, [AncillaryBookingId], _} =
        io_lib:fread(?URI_ANCILLARY_BOOKING_ID_FORMAT, Uri_AncillaryBookingId),
    AncillaryBookingId.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    % some setup here if needed
    % ok = ok,

    [
        {"parsing FlightId",
            [
                ?_assertMatch("111", parse_uri_flight_id("flight-111")),
                ?_assertMatch("flight-111", parse_uri_flight_id("flight-flight-111")),
                ?_assertException(error, {badmatch, _}, parse_uri_flight_id("something-111"))
            ]
        },
        {"parsing FlightDateTime",
            [
                ?_assertMatch({{2013, 8, 29}, {12, 15, 0}}, parse_uri_flight_date_time("2013-08-29T1215Z")),
                ?_assertException(error, {badmatch, _}, parse_uri_flight_date_time("2013-08-29T1215"))
            ]
        },
        {"parsing AncillaryId",
            [
                ?_assertMatch("111", parse_uri_ancillary_id("ancillary-111")),
                ?_assertMatch("ancillary-111", parse_uri_ancillary_id("ancillary-ancillary-111")),
                ?_assertException(error, {badmatch, _}, parse_uri_ancillary_id("something-111"))
            ]
        },
        {"parsing AncillaryBookingId",
            [
                ?_assertMatch("111", parse_uri_ancillary_booking_id("ancillary-booking-111")),
                ?_assertMatch("ancillary-booking-111", parse_uri_ancillary_booking_id("ancillary-booking-ancillary-booking-111")),
                ?_assertException(error, {badmatch, _}, parse_uri_ancillary_booking_id("something-111"))
            ]
        }
    ].

-endif.
