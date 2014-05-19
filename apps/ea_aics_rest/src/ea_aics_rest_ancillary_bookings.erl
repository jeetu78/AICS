%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% ancillary bookings resource
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_ancillary_bookings).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(ea_aics_rest_utils,
    [
        response/2,
        response/1,
        content/2,
        status/1
    ]).

-export([process/4]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include("ea_aics_rest.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Implements HTTP REST on a resource of ancillary-booking type.
%%
%% @end
%%------------------------------------------------------------------------------

-spec process(atom(), string(), {calendar:date(), calendar:time()}, [string()]) -> list().

process(?HTTP_POST, _FlightId, _FlightDateTime, []) ->
    response(content(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY), status(?HTTP_201));
process(?HTTP_GET, _FlightId, _FlightDateTime, []) ->
    response(content(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY), status(?HTTP_200));
process(?HTTP_GET, _FlightId, _FlightDateTime, [Uri_AncillaryBookingId]) ->
    _AncillaryBookingId = ea_aics_rest_utils:parse_uri_ancillary_booking_id(Uri_AncillaryBookingId),
    response(content(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY), status(?HTTP_200));
process(?HTTP_PUT, _FlightId, _FlightDateTime, [Uri_AncillaryBookingId]) ->
    _AncillaryBookingId = ea_aics_rest_utils:parse_uri_ancillary_booking_id(Uri_AncillaryBookingId),
    response(content(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY), status(?HTTP_200));
process(?HTTP_DELETE, _FlightId, _FlightDateTime, [Uri_AncillaryBookingId]) ->
    _AncillaryBookingId = ea_aics_rest_utils:parse_uri_ancillary_booking_id(Uri_AncillaryBookingId),
    response(content(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY), status(?HTTP_200));
process(_Method, _FlightId, _FlightDateTime, _Path) ->
    response(status(?HTTP_405)).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    % some setup here if needed
    % ok = ok,

    [
        {"post",
            [
                ?_assertMatch([{content, _, _}, {status, ?HTTP_201}],
                    process(?HTTP_POST, "111", {"date", "time"}, []))
            ]
        },
        {"get",
            [
                ?_assertMatch([{content, _, _}, {status, ?HTTP_200}],
                    process(?HTTP_GET, "111", {"date", "time"}, [])),
                ?_assertMatch([{content, _, _}, {status, ?HTTP_200}],
                    process(?HTTP_GET, "111", {"date", "time"}, ["ancillary-booking-111"]))
            ]
        },
        {"put",
            [
                ?_assertMatch([{content, _, _}, {status, ?HTTP_200}],
                    process(?HTTP_PUT, "111", {"date", "time"}, ["ancillary-booking-111"]))
            ]
        },
        {"delete",
            [
                ?_assertMatch([{content, _, _}, {status, ?HTTP_200}],
                    process(?HTTP_DELETE, "111", {"date", "time"}, ["ancillary-booking-111"]))
            ]
        },
        {"method not allowed",
            [
                ?_assertMatch([{status, ?HTTP_405}],
                    process(?HTTP_DELETE, "111", {"date", "time"}, []))
            ]
        }
    ].

-endif.
