%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([out/1]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include("ea_aics_rest.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------

%% ===================================================================
%%  yaws callbacks
%% ===================================================================

out(#arg{} = Arg) ->
    Uri = yaws_api:request_url(Arg),
    FullPath = string:tokens(Uri#url.path, "/"),
    HttpRequest = Arg#arg.req,
    HttpRequestMethod = yaws_api:http_request_method(HttpRequest),
    post_process(dispatch(HttpRequestMethod, FullPath)).

%% ===================================================================
%%  Internal Functions
%% ===================================================================

dispatch(Method, ["flights", Uri_FlightId, "dates", Uri_FlightDateTime | Path]) ->
    FlightId = ea_aics_rest_utils:parse_uri_flight_id(Uri_FlightId),
    FlightDateTime = ea_aics_rest_utils:parse_uri_flight_date_time(Uri_FlightDateTime),
    dispatch(Method, FlightId, FlightDateTime, Path);
dispatch(_Method, _Path) ->
    HttpStatus = ?HTTP_STATUS(?HTTP_404),
    [HttpStatus].

dispatch(Method, FlightId, FlightDateTime, ["ancillaries" | Path]) ->
    ea_aics_rest_ancillaries:process(Method, FlightId, FlightDateTime, Path);
dispatch(Method, FlightId, FlightDateTime, ["ancillary-bookings"| Path]) ->
    ea_aics_rest_ancillary_bookings:process(Method, FlightId, FlightDateTime, Path);
dispatch(_Method, _FlightId, _FlightDateTime, _Path) ->
    HttpStatus = ?HTTP_STATUS(?HTTP_404),
    [HttpStatus].

post_process(Result) ->
    Result.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    % some setup here if needed
    % ok = ok,

    [
        {"request pre processing",
            [
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_200)],
                    dispatch(?HTTP_GET, ["flights", "flight-111", "dates", "2013-08-29T1215Z", "ancillaries"])),
                ?_assertMatch([?HTTP_CONTENT(_, _),
                        ?HTTP_STATUS(?HTTP_200)],
                    dispatch(?HTTP_GET, ["flights", "flight-111", "dates", "2013-08-29T1215Z", "ancillary-bookings"])),
                ?_assertMatch([?HTTP_STATUS(?HTTP_404)],
                    dispatch(?HTTP_GET, ["flights", "flight-111", "dates", "2013-08-29T1215Z"])),
                ?_assertMatch([?HTTP_STATUS(?HTTP_404)],
                    dispatch(?HTTP_GET, ["flights", "flight-111", "dates", "2013-08-29T1215Z", "something"])),
                ?_assertMatch([?HTTP_STATUS(?HTTP_404)], dispatch(?HTTP_GET, ["something"]))
            ]
        }
    ].

-endif.
