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

out(#arg{} = WebArg) ->
    Uri = yaws_api:request_url(WebArg),
    Path = string:tokens(Uri#url.path, "/"),
    HttpRequest = WebArg#arg.req,
    HttpRequestMethod = yaws_api:http_request_method(HttpRequest),
    post_process(dispatch(HttpRequestMethod, WebArg, Path)).

%% ===================================================================
%%  Internal Functions
%% ===================================================================

dispatch(Method, WebArg, ["ancillaries" | _RestPath] = Path) ->
    ea_aics_rest_ancillaries:process(Method, WebArg, Path);
dispatch(Method, WebArg, ["flights", _Uri_FlightId, "allocated-ancillaries" | _RestPath] = Path) ->
    ea_aics_rest_flight_allocated_ancillaries:process(Method, WebArg, Path);
dispatch(Method, WebArg, ["flights", _Uri_FlightId, "ancillary-bookings" | _RestPath] = Path) ->
    ea_aics_rest_flight_ancillary_bookings:process(Method, WebArg, Path);
dispatch(_Method, _WebArg, _Path) ->
    HttpStatus = {status, ?HTTP_404},
    [HttpStatus].

post_process(Result) ->
    Result.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    HttpRequestMethod = 'GET',
    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
    HttpRequestHeaders = [],

    {foreach,
     fun()  ->
        ok = meck:new(ea_aics_rest_ancillaries, [non_strict, {stub_all, ok}]),
        ok = meck:new(ea_aics_rest_flight_allocated_ancillaries, [non_strict, {stub_all, ok}]),
        ok = meck:new(ea_aics_rest_flight_ancillary_bookings, [non_strict, {stub_all, ok}])
     end,
     fun(_) ->
        ?assert(meck:validate(ea_aics_rest_ancillaries)),
        ?assert(meck:validate(ea_aics_rest_flight_allocated_ancillaries)),
        ?assert(meck:validate(ea_aics_rest_flight_ancillary_bookings)),
        ok = meck:unload(ea_aics_rest_ancillaries),
        ok = meck:unload(ea_aics_rest_flight_allocated_ancillaries),
        ok = meck:unload(ea_aics_rest_flight_ancillary_bookings)
     end,
     [
        {"request dispatch",
            [
                fun() ->
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),
                    Path = ["ancillaries"],
                    _Response = dispatch(HttpRequestMethod, WebArg, Path),
                    ok = meck:wait(1, ea_aics_rest_ancillaries, process, '_', 1000)
                end
            ]
        },
        {"request dispatch",
            [
                fun() ->
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),
                    Path = ["flights", "111", "allocated-ancillaries", "111"],
                    _Response = dispatch(HttpRequestMethod, WebArg, Path),
                    ok = meck:wait(1, ea_aics_rest_flight_allocated_ancillaries, process, '_', 1000)
                end
            ]
        },
        {"request dispatch",
            [
                fun() ->
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),
                    Path = ["flights", "111", "ancillary-bookings"],
                    _Response = dispatch(HttpRequestMethod, WebArg, Path),
                    ok = meck:wait(1, ea_aics_rest_flight_ancillary_bookings, process, '_', 1000)
                end
            ]
        },
        {"request dispatch",
            [
                fun() ->
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),
                    Path = ["flights", "foobar"],
                    ?assertMatch([{status, ?HTTP_404}], dispatch(HttpRequestMethod, WebArg, Path))
                end
            ]
        },
        {"request dispatch",
            [
                fun() ->
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),
                    Path = ["flights", "111"],
                    ?assertMatch([{status, ?HTTP_404}], dispatch(HttpRequestMethod, WebArg, Path))
                end
            ]
        }
     ]
    }.

-endif.
