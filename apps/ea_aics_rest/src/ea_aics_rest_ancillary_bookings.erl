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
    HttpContent = ?HTTP_CONTENT(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY),
    HttpStatus = ?HTTP_STATUS(?HTTP_201),
    [HttpContent, HttpStatus];
process(?HTTP_GET, _FlightId, _FlightDateTime, []) ->
    HttpContent = ?HTTP_CONTENT(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY),
    HttpStatus = ?HTTP_STATUS(?HTTP_200),
    [HttpContent, HttpStatus];
process(?HTTP_GET, _FlightId, _FlightDateTime, [Uri_AncillaryBookingId]) ->
    _AncillaryBookingId = ea_aics_rest_utils:parse_uri_ancillary_booking_id(Uri_AncillaryBookingId),
    HttpContent = ?HTTP_CONTENT(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY),
    HttpStatus = ?HTTP_STATUS(?HTTP_200),
    [HttpContent, HttpStatus];
process(?HTTP_PUT, _FlightId, _FlightDateTime, [Uri_AncillaryBookingId]) ->
    _AncillaryBookingId = ea_aics_rest_utils:parse_uri_ancillary_booking_id(Uri_AncillaryBookingId),
    HttpContent = ?HTTP_CONTENT(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY),
    HttpStatus = ?HTTP_STATUS(?HTTP_200),
    [HttpContent, HttpStatus];
process(?HTTP_DELETE, _FlightId, _FlightDateTime, [Uri_AncillaryBookingId]) ->
    _AncillaryBookingId = ea_aics_rest_utils:parse_uri_ancillary_booking_id(Uri_AncillaryBookingId),
    HttpContent = ?HTTP_CONTENT(?HTTP_CONTENT_JSON, ?HTTP_BODY_JSON_EMPTY),
    HttpStatus = ?HTTP_STATUS(?HTTP_200),
    [HttpContent, HttpStatus];
process(_Method, _FlightId, _FlightDateTime, _Path) ->
    HttpStatus = ?HTTP_STATUS(?HTTP_405),
    [HttpStatus].

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
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_201)],
                    process(?HTTP_POST, "111", {"date", "time"}, []))
            ]
        },
        {"get",
            [
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_200)],
                    process(?HTTP_GET, "111", {"date", "time"}, [])),
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_200)],
                    process(?HTTP_GET, "111", {"date", "time"}, ["ancillary-booking-111"]))
            ]
        },
        {"put",
            [
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_200)],
                    process(?HTTP_PUT, "111", {"date", "time"}, ["ancillary-booking-111"]))
            ]
        },
        {"delete",
            [
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_200)],
                    process(?HTTP_DELETE, "111", {"date", "time"}, ["ancillary-booking-111"]))
            ]
        },
        {"method not allowed",
            [
                ?_assertMatch([?HTTP_STATUS(?HTTP_405)],
                    process(?HTTP_DELETE, "111", {"date", "time"}, []))
            ]
        }
    ].

-endif.
