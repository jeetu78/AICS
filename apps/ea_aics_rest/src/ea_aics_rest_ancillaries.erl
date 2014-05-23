%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% ancillaries resource
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_ancillaries).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    {ok, Ancillaries} = ea_aics_store_ancillaries:read(),
    JsonView = json_view_ancillaries(WebArg, Path, Ancillaries),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
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

-ifdef(TEST).

module_test_() ->

    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
    HttpRequestHeaders = [],

    {foreach,
     fun()  ->
        ok = meck:new(ea_aics_store_ancillaries, [non_strict])
     end,
     fun(_) ->
        ?assert(meck:validate(ea_aics_store_ancillaries)),
        ok = meck:unload(ea_aics_store_ancillaries)
     end,
     [
        {"post",
            [
                fun() ->
                    ResourceId = <<"111">>,
                    Resource = #ea_aics_ancillary{id = ResourceId},

                    ok = meck:expect(ea_aics_store_ancillaries, create, [], {ok, Resource}),

                    HttpRequestMethod = 'POST',
                    HttpRequestPath = ["ancillaries"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillaries, create, '_', 1000),

                    [HttpResponseStatus, HttpResponseHeaders] = HttpResponse,
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus),
                    ?assertMatch({allheaders, [{header, {"Location", _ResourceInstanceUri}}]}, HttpResponseHeaders)
                end
            ]
        },
        {"get collection",
            [
                fun() ->
                    Resources = [#ea_aics_ancillary{id = <<"111">>},
                                 #ea_aics_ancillary{id = <<"222">>}],

                    ok = meck:expect(ea_aics_store_ancillaries, read, [], {ok, Resources}),

                    HttpRequestMethod = 'GET',
                    HttpRequestPath = ["ancillaries"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillaries, read, '_', 1000),

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
                    Resource = #ea_aics_ancillary{id = ResourceId},

                    ok = meck:expect(ea_aics_store_ancillaries, read, ['_'], {ok, Resource}),

                    HttpRequestMethod = 'GET',
                    HttpRequestPath = ["ancillaries", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillaries, read, '_', 1000),

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
                    HttpRequestPath = ["ancillaries", "111"],
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

                    ok = meck:expect(ea_aics_store_ancillaries, delete, ['_'], ok),

                    HttpRequestMethod = 'DELETE',
                    HttpRequestPath = ["ancillaries", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillaries, delete, '_', 1000),

                    [HttpResponseStatus] = HttpResponse,
                    ?assertMatch({status, _HttpResponseStatusCode}, HttpResponseStatus)
                end
            ]
        },
        {"not allowed",
            [
                fun() ->
                    HttpRequestMethod = 'DELETE',
                    HttpRequestPath = ["ancillaries"],
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
