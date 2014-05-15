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

-import(ea_aics_rest_utils,
    [
        response/2,
        response/1,
        content/2,
        status/1
    ]).

-export([out/1]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").

-define(HTTP_GET, 'GET').
-define(HTTP_POST, 'POST').
-define(HTTP_200, 200).
-define(HTTP_404, 404).
-define(HTTP_405, 405).

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
    Path = string:tokens(Uri#url.path, "/"),
    HttpRequest = Arg#arg.req,
    HttpRequestMethod = yaws_api:http_request_method(HttpRequest),
    process(HttpRequestMethod, Path).

%% ===================================================================
%%  Internal Functions
%% ===================================================================

process(?HTTP_GET, ["flights", Uri_FlightId, "dates", Uri_FlightDateTime]) ->
    _FlightId = ea_aics_rest_utils:parse_uri_flight_id(Uri_FlightId),
    _FlightDateTime = ea_aics_rest_utils:parse_uri_flight_date_time(Uri_FlightDateTime),
    response(content(<<"application/json">>, <<"{}">>), status(?HTTP_200));
process(?HTTP_GET, _Path) ->
    response(status(?HTTP_404));
process(_Method, _Path) ->
    response(status(?HTTP_405)).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    % some setup here if needed
    % ok = ok,

    [
        {"request dispatching",
            [
                ?_assertMatch([{content, _, _}, {status, ?HTTP_200}],
                    process(?HTTP_GET, ["flights", "flight-111", "dates", "2013-08-29T1215Z"])),
                ?_assertMatch([{status, ?HTTP_404}], process(?HTTP_GET, ["something"])),
                ?_assertMatch([{status, ?HTTP_405}], process(?HTTP_POST, ["something"]))
            ]
        }
    ].

-endif.
