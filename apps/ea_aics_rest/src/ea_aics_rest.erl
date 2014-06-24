%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest).


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
dispatch(Method, WebArg, ["book-ancillaries" | _RestPath] = Path) ->
    ea_aics_rest_ancillaries:process(Method, WebArg, Path);
dispatch(_Method, _WebArg, _Path) ->
    HttpStatus = {status, ?HTTP_404},
    [HttpStatus].

post_process(Result) ->
    Result.


