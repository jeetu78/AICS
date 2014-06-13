%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% flight allocated ancillaries resource
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_flight_allocated_ancillaries).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([process/3,
         json_view_allocated_ancillary/4,
         json_view_allocated_ancillaries/4]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").
-include("ea_aics_rest.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Implements HTTP REST on a resource of flight allocated-ancillaries type.
%%
%% @end
%%------------------------------------------------------------------------------

-spec process(atom(), #arg{}, [string()]) -> list().

process('POST', WebArg, ["flights", Uri_FlightId, "allocated-ancillaries"] = Path) ->
    HttpRequestContentBody = WebArg#arg.clidata,
    JsonView = ea_aics_rest_utils:json_decode(HttpRequestContentBody),
    {<<"ancillary">>, JsonView_AncillaryResource} = lists:keyfind(<<"ancillary">>, 1, JsonView),
    {<<"id">>, JsonView_AncillaryId} = lists:keyfind(<<"id">>, 1, JsonView_AncillaryResource),
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    {ok, #ea_aics_allocated_ancillary{} = AllocatedAncillary} =
        ea_aics_store_allocated_ancillaries:create(FlightId, JsonView_AncillaryId, []),
    AllocatedAncillaryId = AllocatedAncillary#ea_aics_allocated_ancillary.id,
    ResourceInstanceUri = resource_instance_uri(WebArg, Path, FlightId, AllocatedAncillaryId),
    HttpStatus = {status, ?HTTP_201},
    HeaderLocation = {"Location", ResourceInstanceUri},
    HttpHeaders = {allheaders, [{header, HeaderLocation}]},
    [HttpStatus, HttpHeaders];
process('GET', WebArg, ["flights", Uri_FlightId, "allocated-ancillaries"] = Path) ->
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    {ok, AllocatedAncillaries} = ea_aics_store_allocated_ancillaries:read(FlightId),
    JsonView = json_view_allocated_ancillaries(WebArg, Path, FlightId, AllocatedAncillaries),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];
process('GET', WebArg, ["flights", Uri_FlightId, "allocated-ancillaries", Uri_AllocatedAncillaryId] = Path) ->
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    AllocatedAncillaryId = ea_aics_rest_utils:parse_uri_id(Uri_AllocatedAncillaryId),
    {ok, AllocatedAncillary} = ea_aics_store_allocated_ancillaries:read(FlightId, AllocatedAncillaryId),
    JsonView = json_view_allocated_ancillary(WebArg, Path, FlightId, AllocatedAncillary),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];
process('PUT', _WebArg, ["flights", _Uri_FlightId, "allocated-ancillaries", _Uri_AllocatedAncillaryId] = _Path) ->
    HttpStatus = {status, ?HTTP_501},
    [HttpStatus];
process('DELETE', _WebArg, ["flights", Uri_FlightId, "allocated-ancillaries", Uri_AllocatedAncillaryId] = _Path) ->
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    AllocatedAncillaryId = ea_aics_rest_utils:parse_uri_id(Uri_AllocatedAncillaryId),
    ok = ea_aics_store_allocated_ancillaries:delete(FlightId, AllocatedAncillaryId),
    HttpStatus = {status, ?HTTP_204},
    [HttpStatus];
process(_Method, _WebArg, _Path) ->
    HttpStatus = {status, ?HTTP_405},
    [HttpStatus].

%%------------------------------------------------------------------------------
%% @doc Allocated-ancillary type resource instance JSON intermediate format.
%%
%% @end
%%------------------------------------------------------------------------------

-spec json_view_allocated_ancillary(#arg{}, [string()], binary(), #ea_aics_allocated_ancillary{}) -> term().

json_view_allocated_ancillary(WebArg, Path, FlightId, #ea_aics_allocated_ancillary{} = AllocatedAncillary) ->
    AllocatedAncillaryId = AllocatedAncillary#ea_aics_allocated_ancillary.id,
    ResourceUri = resource_instance_uri(WebArg, Path, FlightId, AllocatedAncillaryId),
    AllocatedAncillary_Ancillary = AllocatedAncillary#ea_aics_allocated_ancillary.ancillary,
    AllocatedAncillary_AncillaryJsonView =
        ea_aics_rest_ancillaries:json_view_ancillary(WebArg, Path, AllocatedAncillary_Ancillary),
    [{<<"href">>, ResourceUri},
     {<<"ancillary">>, AllocatedAncillary_AncillaryJsonView}].

%%------------------------------------------------------------------------------
%% @doc Allocated-ancillaries type resource collection JSON intermediate format.
%%
%% @end
%%------------------------------------------------------------------------------

-spec json_view_allocated_ancillaries(#arg{}, [string()], binary(), [#ea_aics_ancillary{}]) -> term().

json_view_allocated_ancillaries(WebArg, Path, FlightId, AllocatedAncillaries) when is_list(AllocatedAncillaries) ->
    [{<<"href">>, resource_collection_uri(WebArg, Path, FlightId)},
     {<<"allocatedAncillaries">>, [json_view_allocated_ancillary(WebArg, Path, FlightId, AllocatedAncillary) ||
        AllocatedAncillary <- AllocatedAncillaries]}].

%% ===================================================================
%%  Internal Functions
%% ===================================================================

resource_collection_uri(_WebArg, _Path, FlightId) ->
    % TODO ResourceContext should be managed by web configuration
    ResourceContext = <<(<<"http://localhost/flights">>)/binary, FlightId/binary>>,
    Separator = <<"/">>,
    ResourceCollection = <<"allocated-ancillaries">>,
    <<ResourceContext/binary, Separator/binary, ResourceCollection/binary>>.

resource_instance_uri(WebArg, Path, FlightId, AllocatedAncillaryId) ->
    ResourceCollectionUri = resource_collection_uri(WebArg, Path, FlightId),
    Separator = <<"/">>,
    <<ResourceCollectionUri/binary, Separator/binary, AllocatedAncillaryId/binary>>.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    HttpRequestHeaders = [],

    {foreach,
     fun()  ->
        ok = meck:new(ea_aics_store_allocated_ancillaries, [non_strict])
     end,
     fun(_) ->
        ?assert(meck:validate(ea_aics_store_allocated_ancillaries)),
        ok = meck:unload(ea_aics_store_allocated_ancillaries)
     end,
     [
        {"post",
            [
                fun() ->
                    ResourceId = <<"111">>,
                    Resource = #ea_aics_allocated_ancillary{id = ResourceId},

                    ok = meck:expect(ea_aics_store_allocated_ancillaries, create, ['_', '_', '_'], {ok, Resource}),

                    HttpRequestMethod = 'POST',
                    HttpRequestContentBody = <<"{\"ancillary\": {\"id\": \"111\"}}">>,
                    HttpRequestPath = ["flights", "111", "allocated-ancillaries"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_allocated_ancillaries, create, '_', 1000),

                    [HttpResponseStatus, HttpResponseHeaders] = HttpResponse,
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus),
                    ?assertMatch({allheaders, [{header, {"Location", _ResourceInstanceUri}}]}, HttpResponseHeaders)
                end
            ]
        },
        {"get collection",
            [
                fun() ->
                    Resources = [#ea_aics_allocated_ancillary{id = <<"111">>,
                                                              ancillary = #ea_aics_ancillary{id = <<"111">>}},
                                 #ea_aics_allocated_ancillary{id = <<"222">>,
                                                              ancillary = #ea_aics_ancillary{id = <<"222">>}}],

                    ok = meck:expect(ea_aics_store_allocated_ancillaries, read, ['_'], {ok, Resources}),

                    HttpRequestMethod = 'GET',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "allocated-ancillaries"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_allocated_ancillaries, read, '_', 1000),

                    [HttpResponseContent, HttpResponseStatus] = HttpResponse,
                    ?assertMatch({content, ?HTTP_CONTENT_TYPE_JSON, _HttpResponseContentBody}, HttpResponseContent),
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus)
                end
            ]
        },
        {"get instance",
            [
                fun() ->
                    ResourceId = <<"111">>,
                    Resource = #ea_aics_allocated_ancillary{id = ResourceId,
                                                            ancillary = #ea_aics_ancillary{id = <<"111">>}},

                    ok = meck:expect(ea_aics_store_allocated_ancillaries, read, ['_', '_'], {ok, Resource}),

                    HttpRequestMethod = 'GET',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "allocated-ancillaries", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_allocated_ancillaries, read, '_', 1000),

                    [HttpResponseContent, HttpResponseStatus] = HttpResponse,
                    ?assertMatch({content, ?HTTP_CONTENT_TYPE_JSON, _HttpResponseContentBody}, HttpResponseContent),
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus)
                end
            ]
        },
        {"put",
            [
                fun() ->
                    HttpRequestMethod = 'PUT',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "allocated-ancillaries", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    [HttpResponseStatus] = HttpResponse,
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus)
                end
            ]
        },
        {"delete",
            [
                fun() ->

                    ok = meck:expect(ea_aics_store_allocated_ancillaries, delete, ['_', '_'], ok),

                    HttpRequestMethod = 'DELETE',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "allocated-ancillaries", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_allocated_ancillaries, delete, '_', 1000),

                    [HttpResponseStatus] = HttpResponse,
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus)
                end
            ]
        },
        {"not allowed",
            [
                fun() ->
                    HttpRequestMethod = 'DELETE',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "allocated-ancillaries"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    [HttpResponseStatus] = HttpResponse,
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus)
                end
            ]
        }
     ]
    }.

-endif.
