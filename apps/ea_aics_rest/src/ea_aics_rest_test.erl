%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% test helpers
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_test).

-ifdef(TEST).

-export([web_arg/3,
         web_arg/4]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include("ea_aics_rest.hrl").

web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders) ->
    web_arg(HttpRequestMethod, undefined, HttpRequestContentBody, HttpRequestHeaders).

web_arg(HttpRequestMethod, HttpRequestUri, HttpRequestContentBody, HttpRequestHeaders) ->
    WebArgRequestHeaders =
        lists:foldl(fun({HeaderKey, HeaderValue}, #headers{} = Headers) ->
            yaws_api:set_header(Headers, HeaderKey, HeaderValue)
        end, #headers{}, HttpRequestHeaders),
    #arg{req = #http_request{method = HttpRequestMethod,
                             path = {abs_path, HttpRequestUri}},
         headers = WebArgRequestHeaders,
         clidata = HttpRequestContentBody}.

-endif.
