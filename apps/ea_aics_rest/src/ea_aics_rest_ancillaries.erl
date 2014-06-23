%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% ancillaries resource
%%% @end
%%%=============================================================================
-module(ea_aics_rest_ancillaries).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([process/3,
         validation_spec/0,
         json_view_ancillary/3,
         json_view_ancillaries/3]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").
-include("ea_aics_rest.hrl").

%%=============================================================================
%%  API
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Implements HTTP REST on a resource of ancillary type.
%% @end
%%-----------------------------------------------------------------------------
-spec process(atom(), #arg{}, [string()]) -> list().

process('POST', WebArg, ["ancillaries"] = Path) ->
    Body = yaws_api:arg_clidata(WebArg),
    Json = ea_aics_rest_utils:json_decode(Body),
    case ea_aics_rest_json:parse(validation_spec(), Json) of
        {invalid_json, Reason, JsonFields} ->
            %log an info message with the validation error
            lager:info("Invalid JSON ancillary. Reason: ~p, Input: ~p",
                        [Reason,JsonFields]),
            [{status, ?HTTP_400}];
        Values ->
            {ok, Ancillary} = ea_aics_store_ancillaries:create(json_to_record(Values)),
            JsonView = json_view_ancillary(WebArg, Path, Ancillary),
            HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
            HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
            HttpContent = {content, HttpContentType, HttpContentBody},
            AncillaryId = Ancillary#ea_aics_ancillary.id,
            ResourceInstanceUri = resource_instance_uri(WebArg, Path, AncillaryId),
            HttpStatus = {status, ?HTTP_201},
            HeaderLocation = {"Location", ResourceInstanceUri},
            HttpHeaders = {allheaders, [{header, HeaderLocation}]},
            [HttpContent, HttpStatus, HttpHeaders]
    end;
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
%% @end
%%------------------------------------------------------------------------------
-spec json_view_ancillary(#arg{}, [string()], #ea_aics_ancillary{}) -> term().

json_view_ancillary(WebArg, Path, #ea_aics_ancillary{} = Ancillary) ->
    AncillaryId = Ancillary#ea_aics_ancillary.id,
    ResourceUri = resource_instance_uri(WebArg, Path, AncillaryId),
    [{<<"href">>, ResourceUri},
     {<<"id">>, AncillaryId},
     {<<"masterCode">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.master_code)},
     {<<"serviceProviderId">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.service_provider_id)},
     {<<"subCode">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.sub_code)},
     {<<"groupCode">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.group_code)},
     {<<"subGroup">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.sub_group)},
     {<<"description1">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.description1)},
     {<<"description2">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.description2)},
     {<<"imageThumbnailUrl">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.image_thumbnail_url)},
     {<<"imageLargeUrl">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.image_large_url)},
     {<<"toolTip">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.tooltip)},
     {<<"price">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.price)},
     {<<"currency">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.currency)},
     {<<"tax">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.tax)},
     {<<"isDiscount">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.is_discount)},
     {<<"discountDesc">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.discount_desc)},
     {<<"discountPcnt">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.discount_pcnt)},
     {<<"commercialName">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.commercial_name)},
     {<<"RFIC">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.rfic)},
     {<<"modifiedTime">>,
      ea_aics_rest_utils:record_to_json_value(Ancillary#ea_aics_ancillary.modified_time)}].

%%------------------------------------------------------------------------------
%% @doc Ancillary type resource collection JSON intermediate format.
%% @end
%%------------------------------------------------------------------------------
-spec json_view_ancillaries(#arg{}, [string()], [#ea_aics_ancillary{}]) -> term().

json_view_ancillaries(WebArg, Path, Ancillaries) when is_list(Ancillaries) ->
    [{<<"href">>, resource_collection_uri(WebArg, Path)},
     {<<"ancillaries">>, [json_view_ancillary(WebArg, Path, Ancillary)
        || Ancillary <- Ancillaries]}].

%%=============================================================================
%%  Internal Functions
%%=============================================================================

resource_collection_uri(_WebArg, _Path) ->
    % TODO ResourceContext should be managed by web configuration
    ResourceContext = <<"http://localhost:8000">>,
    Separator = <<"/">>,
    ResourceCollection = <<"ancillaries">>,
    <<ResourceContext/binary, Separator/binary, ResourceCollection/binary>>.

resource_instance_uri(WebArg, Path, AncillaryId) ->
    ResourceCollectionUri = resource_collection_uri(WebArg, Path),
    Separator = <<"/">>,
    <<ResourceCollectionUri/binary, Separator/binary, AncillaryId/binary>>.

validation_spec() ->
    [{<<"masterCode">>, optional, integer},
     {<<"serviceProviderId">>, optional, string},
     {<<"subCode">>, optional, string},
     {<<"groupCode">>, optional, string},
     {<<"subGroup">>, optional, string},
     {<<"description1">>, optional, string},
     {<<"description2">>, optional, string},
     {<<"imageThumbnailUrl">>, optional, string},
     {<<"imageLargeUrl">>, optional, string},
     {<<"toolTip">>, optional, string},
     {<<"price">>, optional, float},
     {<<"currency">>, optional, string},
     {<<"tax">>, optional, float},
     {<<"isDiscount">>, optional, string},
     {<<"discountDesc">>, optional, string},
     {<<"iscountPcnt">>, optional, float},
     {<<"commercialName">>, optional, string},
     {<<"RFIC">>, optional, string}].

json_to_record(JsonInput) ->
    #ea_aics_ancillary{
        master_code = proplists:get_value(<<"masterCode">>, JsonInput),
        service_provider_id = proplists:get_value(<<"serviceProviderId">>,
                                                  JsonInput),
        sub_code = proplists:get_value(<<"subCode">>, JsonInput),
        group_code = proplists:get_value(<<"groupCode">>, JsonInput),
        sub_group = proplists:get_value(<<"subGroup">>, JsonInput),
        description1 = proplists:get_value(<<"description1">>, JsonInput),
        description2 = proplists:get_value(<<"description2">>, JsonInput),
        image_thumbnail_url = proplists:get_value(<<"imageThumbnailUrl">>,
                                                  JsonInput),
        image_large_url = proplists:get_value(<<"imageLargeUrl">>, JsonInput),
        tooltip = proplists:get_value(<<"toolTip">>, JsonInput),
        price = proplists:get_value(<<"price">>, JsonInput),
        currency = proplists:get_value(<<"currency">>, JsonInput),
        tax = proplists:get_value(<<"tax">>, JsonInput),
        is_discount = proplists:get_value(<<"isDiscount">>, JsonInput),
        discount_desc = proplists:get_value(<<"discountDesc">>, JsonInput),
        discount_pcnt = proplists:get_value(<<"iscountPcnt">>, JsonInput),
        commercial_name = proplists:get_value(<<"commercialName">>, JsonInput),
        rfic = proplists:get_value(<<"RFIC">>, JsonInput)}.

%%=============================================================================
%%  Tests
%%=============================================================================

-ifdef(TEST).

module_test_() ->

    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
    EmptyJson = <<"[]">>,
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

                    ok = meck:expect(ea_aics_store_ancillaries, create, ['_'], {ok, Resource}),

                    HttpRequestMethod = 'POST',
                    HttpRequestPath = ["ancillaries"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, EmptyJson, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillaries, create, '_', 1000),

                    [HttpResponseContent, HttpResponseStatus, HttpResponseHeaders] = HttpResponse,
                    ?assertMatch({content, ?HTTP_CONTENT_TYPE_JSON, _HttpResponseContentBody}, HttpResponseContent),
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
