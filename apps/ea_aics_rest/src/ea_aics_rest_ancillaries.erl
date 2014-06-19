%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% ancillaries resource
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_ancillaries).


-export([process/3,
         json_view_ancillary/3,
         json_view_ancillaries/3]).

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

process('POST', WebArg, ["ancillaries"] = Path) ->
    {ok, #ea_aics_ancillary{} = Ancillary} = ea_aics_store_ancillaries:create(),
    AncillaryId = Ancillary#ea_aics_ancillary.id,
    ResourceInstanceUri = resource_instance_uri(WebArg, Path, AncillaryId),
    HttpStatus = {status, ?HTTP_201},
    HeaderLocation = {"Location", ResourceInstanceUri},
    HttpHeaders = {allheaders, [{header, HeaderLocation}]},
    [HttpStatus, HttpHeaders];
process('GET', WebArg, ["ancillaries"] = Path) ->
    io:format("~p~p",[WebArg,Path]),
    AncillariesJsonView= ea_aics_service:getAncillariesForFlight("857c16d7-acb7-4a87-98dc-641364a88779"),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(AncillariesJsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    %%HttpContent = {content, "text/HTML", "this is body"},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];

process('GET', WebArg, ["book-ancillaries"] = Path) ->
    io:format("~p~p",[WebArg,Path]),
	Json=list_to_binary("{ \"data\":[ { \"anc_inv_uuid\": \"009171d0-561e-4d72-8297-0ec75da4c274\", \"customer_uuid\": \"1234\", \"quantity\" : 2 }, { \"anc_inv_uuid\": \"fe992fc5-5862-4f69-bf26-f13648dfb128\", \"customer_uuid\": \"1234\", \"quantity\" : 3 }, { \"anc_inv_uuid\": \"f2fa31ba-0e87-40b5-a4c1-06d50651e571\", \"customer_uuid\": \"xyz\", \"quantity\" : 1 } ] }"),
	JsonStructure=ea_aics_rest_utils:json_decode(Json),
    TxJsonView= ea_aics_service:bookAncillaries(JsonStructure),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(TxJsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    %%HttpContent = {content, "text/HTML", "this is body"},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];

process('GET', WebArg, ["ancillaries", Uri_AncillaryId] = Path) ->
    AncillaryId = ea_aics_rest_utils:parse_uri_id(Uri_AncillaryId),
    {ok, Ancillary} = ea_aics_store_ancillaries:read(AncillaryId),
    JsonView = json_view_ancillary(WebArg, Path, Ancillary),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];
process('PUT', _WebArg, ["ancillaries", _Uri_AncillaryId] = _Path) ->
    HttpStatus = {status, ?HTTP_501},
    [HttpStatus];
process('DELETE', _WebArg, ["ancillaries", Uri_AncillaryId] = _Path) ->
    ok = ea_aics_store_ancillaries:delete(Uri_AncillaryId),
    HttpStatus = {status, ?HTTP_204},
    [HttpStatus];
process(_Method, _WebArg, _Path) ->
    HttpStatus = {status, ?HTTP_405},
    [HttpStatus].

%%------------------------------------------------------------------------------
%% @doc Ancillary type resource instance JSON intermediate format.
%%
%% @end
%%------------------------------------------------------------------------------

-spec json_view_ancillary(#arg{}, [string()], #ea_aics_ancillary{}) -> term().

json_view_ancillary(WebArg, Path, #ea_aics_ancillary{} = Ancillary) ->
    AncillaryId = Ancillary#ea_aics_ancillary.id,
    ResourceUri = resource_instance_uri(WebArg, Path, AncillaryId),
    [{<<"href">>, ResourceUri}].

%%------------------------------------------------------------------------------
%% @doc Ancillary type resource collection JSON intermediate format.
%%
%% @end
%%------------------------------------------------------------------------------

-spec json_view_ancillaries(#arg{}, [string()], [#ea_aics_ancillary{}]) -> term().

json_view_ancillaries(WebArg, Path, Ancillaries) when is_list(Ancillaries) ->
    [{<<"href">>, resource_collection_uri(WebArg, Path)},
     {<<"ancillaries">>, [json_view_ancillary(WebArg, Path, Ancillary)
        || Ancillary <- Ancillaries]}].

%% ===================================================================
%%  Internal Functions
%% ===================================================================

resource_collection_uri(_WebArg, _Path) ->
    % TODO ResourceContext should be managed by web configuration
    ResourceContext = <<"http://localhost">>,
    Separator = <<"/">>,
    ResourceCollection = <<"ancillaries">>,
    <<ResourceContext/binary, Separator/binary, ResourceCollection/binary>>.

resource_instance_uri(WebArg, Path, AncillaryId) ->
    ResourceCollectionUri = resource_collection_uri(WebArg, Path),
    Separator = <<"/">>,
    <<ResourceCollectionUri/binary, Separator/binary, AncillaryId/binary>>.

%% ===================================================================
%%  Tests
%% ===================================================================

