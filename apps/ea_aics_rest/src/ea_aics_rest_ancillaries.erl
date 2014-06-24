%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% ancillaries resource
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_ancillaries).


-export([process/3]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").
-include("ea_aics_rest.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Implements HTTP REST on a resource of ancillary type.
%%
%% @end
%%------------------------------------------------------------------------------

-spec process(atom(), #arg{}, [string()]) -> list().

process('GET', WebArg, ["ancillaries"] = Path) ->
    io:format("~p~p",[WebArg,Path]),
	AncillariesJsonView=case lists:keysearch("flight-uuid", 1, yaws_api:parse_query(WebArg)) of
							{value, {"flight-uuid", Flight_UUID}} ->
								case Flight_UUID of
									undefined->
										ea_aics_service:createInputForJsonEncode(error,"Empty flight-uuid value");
									Flight_UUID->
										ea_aics_service:getAncillariesForFlight(Flight_UUID)
								end;
							false ->
								ea_aics_service:createInputForJsonEncode(error,"No flight-uuid parameter")
						end,
	HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
	HttpContentBody = ea_aics_rest_utils:json_encode(AncillariesJsonView),
	HttpContent = {content, HttpContentType, HttpContentBody},
	%%HttpContent = {content, "text/HTML", HttpContentBody},
	HttpStatus = {status, ?HTTP_200},
	[HttpContent, HttpStatus];    

process('POST', WebArg, ["book-ancillaries"] = Path) ->
    io:format("~p~p",[WebArg,Path]),
    #arg{clidata=Client_Data}=WebArg,
    JsonStructure=ea_aics_rest_utils:json_decode(Client_Data),
    TxJsonView= ea_aics_service:bookAncillaries(JsonStructure),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(TxJsonView),
    HttpContent = {content, HttpContentType,HttpContentBody},
    %%HttpContent = {content, "text/HTML", HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];
process(_Method, _WebArg, _Path) ->
    HttpStatus = {status, ?HTTP_405},
    [HttpStatus].

