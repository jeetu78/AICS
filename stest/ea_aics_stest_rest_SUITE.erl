-module(ea_aics_stest_rest_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(UTIL, ea_aics_stest).
-define(HOST, <<"http://localhost:8000">>).
-define(NODE, 'ea_aics@127.0.0.1').
-define(ITERATIONS, 1).

all() -> [{group, stest}].

groups() -> [{stest, [parallel, {repeat, ?ITERATIONS}], [{group, session}]},
             {session, [], [create_ancillary,
                            read_ancillary,
                            read_ancillaries,
                            create_allocated_ancillary,
                            read_allocated_ancillary,
                            read_allocated_ancillaries,
                            create_ancillary_booking,
                            read_ancillary_booking,
                            read_ancillary_bookings,
                            delete_ancillary_booking,
                            delete_allocated_ancillary,
                            delete_ancillary]}].

suite() -> [].

init_per_suite(Config) ->
    ct:timetrap(10000),
    os:cmd("../../../_rel/ea_aics/bin/ea_aics start"),
    true = ?UTIL:wait_alive(?NODE, 'ea_aics'),
    ct:pal("~n~p started~n", [?NODE]),
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(lhttpc),
    timer:sleep(5000),
    Config.

end_per_suite(Config) ->
    ct:timetrap(10000),
    ok = application:stop(lhttpc),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(crypto),
    ok = application:stop(asn1),
    os:cmd("../../../_rel/ea_aics/bin/ea_aics stop"),
    true = ?UTIL:wait_dead(?NODE, 'ea_aics'),
    ct:pal("~n~p stopped~n", [?NODE]).

init_per_group(session, Config) ->
    ct:timetrap(10000),
    {ok, Client} = ?UTIL:connect_client(binary_to_list(?HOST)),
    true = erlang:unlink(Client),
    DataOwner = spawn(fun ?UTIL:data_owner/0),
    true = erlang:unlink(DataOwner),
    TestData = ets:new(list_to_atom(erlang:ref_to_list(make_ref())),
        [set, public, {heir, DataOwner, []}]),
    [{client, Client}, {data_owner, DataOwner}, {test_data, TestData} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(session, Config) ->
    DataOwner = ?config(data_owner, Config),
    DataOwner ! stop;
end_per_group(_, Config) ->
    ok.

create_ancillary(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    RequestBody = jsx:encode([{<<"masterCode">>, 111}]),
    AncillaryCollectionUri = <<?HOST/binary, (<<"/ancillaries">>)/binary>>,
    {ok, Response} = lhttpc:request_client(Client, AncillaryCollectionUri,
        post, [], RequestBody, 10000),
    ?assertEqual({{201, "Created"}, Response}, {?UTIL:status(Response), Response}),
    ResponseHeaders = ?UTIL:headers(Response),
    {"Location", AncillaryLocation} = lists:keyfind("Location", 1, ResponseHeaders),
    true = ?UTIL:data_insert(TestData, ancillary_location, AncillaryLocation),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AncillaryHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"id">>, AncillaryId} = lists:keyfind(<<"id">>, 1, ResponseObject),
    true = ?UTIL:data_insert(TestData, ancillary_id, AncillaryId).

read_ancillary(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryLocation = ?UTIL:data_lookup(TestData, ancillary_location),
    {ok, Response} = lhttpc:request_client(Client, AncillaryLocation,
        get, [], [], 10000),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AncillaryHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"id">>, AncillaryId} = lists:keyfind(<<"id">>, 1, ResponseObject),
    ?assertEqual({{200, "OK"}, Response}, {?UTIL:status(Response), Response}).

read_ancillaries(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryCollectionUri = <<"http://localhost:8000/ancillaries">>,
    {ok, Response} = lhttpc:request_client(Client, AncillaryCollectionUri,
        get, [], [], 10000),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AncillariesHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"ancillaries">>, _Ancillaries} = lists:keyfind(<<"ancillaries">>, 1, ResponseObject),
    ?assertEqual({{200, "OK"}, Response}, {?UTIL:status(Response), Response}).

delete_ancillary(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryLocation = ?UTIL:data_lookup(TestData, ancillary_location),
    {ok, Response} = lhttpc:request_client(Client, AncillaryLocation,
        delete, [], [], 10000),
    ?assertEqual({{204, "No Content"}, Response}, {?UTIL:status(Response), Response}).

create_allocated_ancillary(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryId = ?UTIL:data_lookup(TestData, ancillary_id),
    RequestBody = jsx:encode([{<<"inventoryId">>, 111},
                              {<<"ancillary">>, [{<<"id">>, AncillaryId}]}]),
    AllocatedAncillaryCollectionUri = <<?HOST/binary,
                                        (<<"/flights/111/allocated-ancillaries">>)/binary>>,
    {ok, Response} = lhttpc:request_client(Client, AllocatedAncillaryCollectionUri,
        post, [], RequestBody, 10000),
    ?assertEqual({{201, "Created"}, Response}, {?UTIL:status(Response), Response}),
    ResponseHeaders = ?UTIL:headers(Response),
    {"Location", AllocatedAncillaryLocation} =
        lists:keyfind("Location", 1, ResponseHeaders),
    true = ?UTIL:data_insert(TestData, allocated_ancillary_location, AllocatedAncillaryLocation),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AllocatedAncillaryHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"id">>, AllocatedAncillaryId} = lists:keyfind(<<"id">>, 1, ResponseObject),
    true = ?UTIL:data_insert(TestData, allocated_ancillary_id, AllocatedAncillaryId).

read_allocated_ancillary(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AllocatedAncillaryLocation = ?UTIL:data_lookup(TestData, allocated_ancillary_location),
    {ok, Response} = lhttpc:request_client(Client, AllocatedAncillaryLocation,
        get, [], [], 10000),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AllocatedAncillaryHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"id">>, AllocatedAncillaryId} = lists:keyfind(<<"id">>, 1, ResponseObject),
    ?assertEqual({{200, "OK"}, Response}, {?UTIL:status(Response), Response}).

read_allocated_ancillaries(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AllocatedAncillaryCollectionUri = <<?HOST/binary,
                                        (<<"/flights/111/allocated-ancillaries">>)/binary>>,
    {ok, Response} = lhttpc:request_client(Client, AllocatedAncillaryCollectionUri,
        get, [], [], 10000),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AllocatedAncillariesHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"allocatedAncillaries">>, _AllocatedAncillaries} =
        lists:keyfind(<<"allocatedAncillaries">>, 1, ResponseObject),
    ?assertEqual({{200, "OK"}, Response}, {?UTIL:status(Response), Response}).

delete_allocated_ancillary(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AllocatedAncillaryLocation = ?UTIL:data_lookup(TestData, allocated_ancillary_location),
    {ok, Response} = lhttpc:request_client(Client, AllocatedAncillaryLocation,
        delete, [], [], 10000),
    ?assertEqual({{204, "No Content"}, Response}, {?UTIL:status(Response), Response}).

create_ancillary_booking(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AllocatedAncillaryId = ?UTIL:data_lookup(TestData, allocated_ancillary_id),
    RequestBody = jsx:encode([{<<"txnId">>, 111},
                              {<<"customerId">>, <<"111">>},
                              {<<"quantity">>, 1},
                              {<<"allocatedAncillary">>, [{<<"id">>, AllocatedAncillaryId}]}]),
    AncillaryBookingCollectionUri = <<?HOST/binary,
                                      (<<"/flights/111/ancillary-bookings">>)/binary>>,
    {ok, Response} = lhttpc:request_client(Client, AncillaryBookingCollectionUri,
        post, [], RequestBody, 10000),
    ?assertEqual({{201, "Created"}, Response}, {?UTIL:status(Response), Response}),
    ResponseHeaders = ?UTIL:headers(Response),
    {"Location", AncillaryBookingLocation} =
        lists:keyfind("Location", 1, ResponseHeaders),
    true = ?UTIL:data_insert(TestData, ancillary_booking_location, AncillaryBookingLocation),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AncillaryBookingHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"id">>, AncillaryBookingId} = lists:keyfind(<<"id">>, 1, ResponseObject),
    true = ?UTIL:data_insert(TestData, ancillary_booking_id, AncillaryBookingId).

read_ancillary_booking(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryBookingLocation = ?UTIL:data_lookup(TestData, ancillary_booking_location),
    {ok, Response} = lhttpc:request_client(Client, AncillaryBookingLocation,
        get, [], [], 10000),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AncillaryBookingHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"id">>, AncillaryBookingId} = lists:keyfind(<<"id">>, 1, ResponseObject),
    ?assertEqual({{200, "OK"}, Response}, {?UTIL:status(Response), Response}).

read_ancillary_bookings(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryBookingCollectionUri =
        <<"http://localhost:8000/flights/111/ancillary-bookings">>,
    {ok, Response} = lhttpc:request_client(Client, AncillaryBookingCollectionUri,
        get, [], [], 10000),
    ResponseBody = ?UTIL:body(Response),
    ResponseObject = jsx:decode(ResponseBody),
    {<<"href">>, _AncillaryBookingsHref} = lists:keyfind(<<"href">>, 1, ResponseObject),
    {<<"ancillaryBookings">>, _AncillaryBookings} =
        lists:keyfind(<<"ancillaryBookings">>, 1, ResponseObject),
    ?assertEqual({{200, "OK"}, Response}, {?UTIL:status(Response), Response}).

delete_ancillary_booking(Config) ->
    Client = ?config(client, Config),
    TestData = ?config(test_data, Config),
    AncillaryBookingLocation = ?UTIL:data_lookup(TestData, ancillary_booking_location),
    {ok, Response} = lhttpc:request_client(Client, AncillaryBookingLocation,
        delete, [], [], 10000),
    ?assertEqual({{204, "No Content"}, Response}, {?UTIL:status(Response), Response}).
