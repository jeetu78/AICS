%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface
%%% flight ancillary bookings resource
%%% @end
%%%=============================================================================
-module(ea_aics_rest_flight_ancillary_bookings).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([process/3,
         validation_spec/0]).

-export_type([]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").
-include("ea_aics_rest.hrl").

%%=============================================================================
%%  API
%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Implements HTTP REST on a resource of ancillary-booking type.
%% @end
%%------------------------------------------------------------------------------
-spec process(atom(), #arg{}, [string()]) -> list().

process('POST', WebArg, ["flights", Uri_FlightId, "ancillary-bookings"] = Path) ->
    Body = yaws_api:arg_clidata(WebArg),
    Json = ea_aics_rest_utils:json_decode(Body),
    case ea_aics_rest_json:parse(validation_spec(), Json) of
        {invalid_json, Reason, JsonFields} ->
            lager:info("Invalid JSON ancillary-bookings. Reason: ~p, Input: ~p",
                       [Reason,JsonFields]),
            [{status, ?HTTP_400}];
        Values ->
            FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
            AncillaryBookingInput = json_to_record(Values),
            {ok, #ea_aics_ancillary_booking{} = AncillaryBooking}
                = ea_aics_store_ancillary_bookings:create(FlightId,
                                                          AncillaryBookingInput),
            ok = ea_aics_mq:produce(AncillaryBooking),
            JsonView = json_view_ancillary_booking(WebArg, Path, FlightId,
                                                   AncillaryBooking),
            HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
            HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
            HttpContent = {content, HttpContentType, HttpContentBody},
            AncillaryBookingId = AncillaryBooking#ea_aics_ancillary_booking.id,
            ResourceInstanceUri = resource_instance_uri(WebArg, Path, FlightId,
                                                        AncillaryBookingId),
            HttpStatus = {status, ?HTTP_201},
            HeaderLocation = {"Location", ResourceInstanceUri},
            HttpHeaders = {allheaders, [{header, HeaderLocation}]},
            [HttpContent, HttpStatus, HttpHeaders]
    end;
process('GET', WebArg, ["flights", Uri_FlightId, "ancillary-bookings"] = Path) ->
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    {ok, AncillaryBookings} = ea_aics_store_ancillary_bookings:read(FlightId),
    JsonView = json_view_ancillary_bookings(WebArg, Path, FlightId, AncillaryBookings),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];
process('GET', WebArg, ["flights", Uri_FlightId, "ancillary-bookings", Uri_AncillaryBookingId] = Path) ->
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    AncillaryBookingId = ea_aics_rest_utils:parse_uri_id(Uri_AncillaryBookingId),
    {ok, AncillaryBooking} = ea_aics_store_ancillary_bookings:read(FlightId, AncillaryBookingId),
    JsonView = json_view_ancillary_booking(WebArg, Path, FlightId, AncillaryBooking),
    HttpContentType = ?HTTP_CONTENT_TYPE_JSON,
    HttpContentBody = ea_aics_rest_utils:json_encode(JsonView),
    HttpContent = {content, HttpContentType, HttpContentBody},
    HttpStatus = {status, ?HTTP_200},
    [HttpContent, HttpStatus];
process('PUT', _WebArg, ["flights", _Uri_FlightId, "ancillary-bookings", _Uri_AncillaryBookingId] = _Path) ->
    HttpStatus = {status, ?HTTP_501},
    [HttpStatus];
process('DELETE', _WebArg, ["flights", Uri_FlightId, "ancillary-bookings", Uri_AncillaryBookingId] = _Path) ->
    FlightId = ea_aics_rest_utils:parse_uri_id(Uri_FlightId),
    AncillaryBookingId = ea_aics_rest_utils:parse_uri_id(Uri_AncillaryBookingId),
    ok = ea_aics_store_ancillary_bookings:delete(FlightId, AncillaryBookingId),
    HttpStatus = {status, ?HTTP_204},
    [HttpStatus];
process(_Method, _WebArg, _Path) ->
    HttpStatus = {status, ?HTTP_405},
    [HttpStatus].

%%------------------------------------------------------------------------------
%% @doc Ancillary-booking type resource instance JSON intermediate format.
%% @end
%%------------------------------------------------------------------------------

-spec json_view_ancillary_booking(#arg{}, [string()], binary(), #ea_aics_ancillary_booking{}) -> term().

json_view_ancillary_booking(WebArg, Path, FlightId, #ea_aics_ancillary_booking{} = AncillaryBooking) ->
    AncillaryBookingId = AncillaryBooking#ea_aics_ancillary_booking.id,
    ResourceUri = resource_instance_uri(WebArg, Path, FlightId, AncillaryBookingId),
    AncillaryBooking_AllocatedAncillary = AncillaryBooking#ea_aics_ancillary_booking.allocated_ancillary,
    AncillaryBooking_AllocatedAncillaryJsonView =
        ea_aics_rest_flight_allocated_ancillaries:json_view_allocated_ancillary(WebArg,
            Path, FlightId, AncillaryBooking_AllocatedAncillary),
    [{<<"href">>, ResourceUri},
     {<<"id">>, AncillaryBookingId},
     {<<"txnId">>,
      ea_aics_rest_utils:record_to_json_value(AncillaryBooking#ea_aics_ancillary_booking.txn_id)},
     {<<"customerId">>,
      ea_aics_rest_utils:record_to_json_value(AncillaryBooking#ea_aics_ancillary_booking.customer_id)},
     {<<"operationType">>,
      ea_aics_rest_utils:record_to_json_value(AncillaryBooking#ea_aics_ancillary_booking.operation_type)},
     {<<"bookingTime">>,
      ea_aics_rest_utils:record_to_json_value(AncillaryBooking#ea_aics_ancillary_booking.booking_time)},
     {<<"quantity">>,
      ea_aics_rest_utils:record_to_json_value(AncillaryBooking#ea_aics_ancillary_booking.quantity)},
     {<<"modifiedTime">>,
      ea_aics_rest_utils:record_to_json_value(AncillaryBooking#ea_aics_ancillary_booking.modified_time)},
     {<<"allocatedAncillary">>, AncillaryBooking_AllocatedAncillaryJsonView}].

%%------------------------------------------------------------------------------
%% @doc Ancillary-bookings type resource collection JSON intermediate format.
%% @end
%%------------------------------------------------------------------------------

-spec json_view_ancillary_bookings(#arg{}, [string()], binary(), [#ea_aics_ancillary_booking{}]) -> term().

json_view_ancillary_bookings(WebArg, Path, FlightId, AncillaryBookings) when is_list(AncillaryBookings) ->
    [{<<"href">>, resource_collection_uri(WebArg, Path, FlightId)},
     {<<"ancillaryBookings">>, [json_view_ancillary_booking(WebArg, Path, FlightId, AncillaryBooking) ||
        AncillaryBooking <- AncillaryBookings]}].

%% ===================================================================
%%  Internal Functions
%% ===================================================================

resource_collection_uri(_WebArg, _Path, FlightId) ->
    % TODO ResourceContext should be managed by web configuration
    Separator = <<"/">>,
    ResourceContext = <<(<<"http://localhost:8000/flights">>)/binary,
                         Separator/binary,
                         FlightId/binary>>,
    ResourceCollection = <<"ancillary-bookings">>,
    <<ResourceContext/binary, Separator/binary, ResourceCollection/binary>>.

resource_instance_uri(WebArg, Path, FlightId, AncillaryBookingId) ->
    ResourceCollectionUri = resource_collection_uri(WebArg, Path, FlightId),
    Separator = <<"/">>,
    <<ResourceCollectionUri/binary, Separator/binary, AncillaryBookingId/binary>>.

validation_spec() ->
    [{<<"ancTxnId">>, optional, integer},
     {<<"operationType">>, optional, string},
     {<<"bookingTime">>, optional, string},
     {<<"quantity">>, optional, integer},
     {<<"customerId">>, optional, string},
     %% TODO: At the moment we have not implemented the possibilty of
     %% dinamically create an allocated ancillary object on this request.
     %% Therefore the allocated ancillary object now can only be a simple "id".
     %% It must be already created beforehand.
     {<<"allocatedAncillary">>, mandatory,
      {json, [{<<"id">>, mandatory, binary}]}}].

json_to_record(JsonInput) ->
    AllocatedAncillary = proplists:get_value(<<"allocatedAncillary">>,
                                             JsonInput),
    Id = proplists:get_value(<<"Id">>, AllocatedAncillary),
    #ea_aics_ancillary_booking{
        customer_id = proplists:get_value(<<"customerId">>, JsonInput),
        txn_id = proplists:get_value(<<"ancTxnId">>, JsonInput),
        operation_type = proplists:get_value(<<"operationType">>, JsonInput),
        booking_time = proplists:get_value(<<"bookingTime">>, JsonInput),
        quantity = proplists:get_value(<<"quantity">>, JsonInput),
        allocated_ancillary = Id}.

%%=============================================================================
%%  Tests
%%=============================================================================

-ifdef(TEST).

module_test_() ->

    HttpRequestHeaders = [],

    {foreach,
     fun()  ->
        ok = meck:new(ea_aics_store_ancillary_bookings, [non_strict])
     end,
     fun(_) ->
        ?assert(meck:validate(ea_aics_store_ancillary_bookings)),
        ok = meck:unload(ea_aics_store_ancillary_bookings)
     end,
     [
        {"post",
            [
                fun() ->

                    ok = meck:new(ea_aics_mq, [non_strict]),

                    ok = meck:expect(ea_aics_mq, produce, ['_'], ok),

                    Ancillary = #ea_aics_ancillary{id = <<"111">>},
                    AllocatedAncillary = #ea_aics_allocated_ancillary{id = <<"111">>,
                                                                      ancillary = Ancillary},
                    AncillaryBooking = #ea_aics_ancillary_booking{id = <<"111">>,
                                                                  allocated_ancillary = AllocatedAncillary},

                    ok = meck:expect(ea_aics_store_ancillary_bookings, create, ['_', '_'], {ok, AncillaryBooking}),

                    HttpRequestMethod = 'POST',
                    HttpRequestContentBody = <<"{\"allocatedAncillary\": {\"id\": \"111\"}}">>,
                    HttpRequestPath = ["flights", "111", "ancillary-bookings"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillary_bookings, create, '_', 1000),

                    ok = meck:wait(ea_aics_mq, produce, '_', 1000),

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
                    Resources = [#ea_aics_ancillary_booking{id = <<"111">>,
                                                            allocated_ancillary = #ea_aics_allocated_ancillary{id = <<"111">>,
                                                                                                               ancillary = #ea_aics_ancillary{id = <<"111">>}}},
                                 #ea_aics_ancillary_booking{id = <<"222">>,
                                                            allocated_ancillary = #ea_aics_allocated_ancillary{id = <<"222">>,
                                                                                                               ancillary = #ea_aics_ancillary{id = <<"222">>}}}],

                    ok = meck:expect(ea_aics_store_ancillary_bookings, read, ['_'], {ok, Resources}),

                    HttpRequestMethod = 'GET',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "ancillary-bookings"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillary_bookings, read, '_', 1000),

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
                    Resource = #ea_aics_ancillary_booking{id = ResourceId,
                                                          allocated_ancillary = #ea_aics_allocated_ancillary{id = <<"111">>,
                                                                                                             ancillary = #ea_aics_ancillary{id = <<"111">>}}},

                    ok = meck:expect(ea_aics_store_ancillary_bookings, read, ['_', '_'], {ok, Resource}),

                    HttpRequestMethod = 'GET',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "ancillary-bookings", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillary_bookings, read, '_', 1000),

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
                    HttpRequestPath = ["flights", "111", "ancillary-bookings", "111"],
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

                    ok = meck:expect(ea_aics_store_ancillary_bookings, delete, ['_', '_'], ok),

                    HttpRequestMethod = 'DELETE',
                    HttpRequestContentBody = ?HTTP_CONTENT_BODY_EMPTY,
                    HttpRequestPath = ["flights", "111", "ancillary-bookings", "111"],
                    WebArg = ea_aics_rest_test:web_arg(HttpRequestMethod, HttpRequestContentBody, HttpRequestHeaders),

                    HttpResponse = process(HttpRequestMethod, WebArg, HttpRequestPath),

                    ok = meck:wait(ea_aics_store_ancillary_bookings, delete, '_', 1000),

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
                    HttpRequestPath = ["flights", "111", "ancillary-bookings"],
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
