%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System
%%% ancillary bookings store interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_ancillary_bookings).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create/2,
         read/1,
         read/2,
         update/3,
         delete/2,
         record_fields_keys/1,
         result_fields_keys/0,
         parse_query_result_row/1]).

-export_type([]).

-include_lib("mysql/include/mysql.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Creates an ancillary booking in the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec create(binary(), #ea_aics_ancillary_booking{}) -> {ok, #ea_aics_ancillary_booking{}}.

create(FlightId, AncillaryBookingInput) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_create_transaction(ConnectionPid, FlightId, AncillaryBookingInput)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillary bookings from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read(binary()) -> {ok, [#ea_aics_ancillary_booking{}]}.

read(FlightId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid, FlightId)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillary booking from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read(binary(), binary()) -> {ok, #ea_aics_ancillary_booking{}} | {error, not_found}.

read(FlightId, AncillaryBookingId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid, FlightId, AncillaryBookingId)
        end).

%%------------------------------------------------------------------------------
%% @doc Updates the ancillary booking in the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec update(binary(), binary(), term()) -> {ok, #ea_aics_ancillary_booking{}} | {error, not_found}.

update(FlightId, AncillaryBookingId, AncillaryBookingUpdates) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_update_transaction(ConnectionPid, FlightId, AncillaryBookingId,
                AncillaryBookingUpdates)
        end).

%%------------------------------------------------------------------------------
%% @doc Deletes the ancillary booking from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete(binary(), binary()) -> ok | {error, not_found}.

delete(FlightId, AncillaryBookingId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_delete(ConnectionPid, FlightId, AncillaryBookingId)
        end).

%%------------------------------------------------------------------------------
%% @doc Exports the ancillary booking record fields structure.
%% @end
%%------------------------------------------------------------------------------

-spec record_fields_keys(atom()) -> [{atom(), atom()}].

record_fields_keys(Prefix) ->
    [{Prefix, Key} || Key <- record_fields_keys()].

%%------------------------------------------------------------------------------
%% @doc Exports the ancillary booking result fields structure.
%% @end
%%------------------------------------------------------------------------------

-spec result_fields_keys() -> [{atom(), atom()}].

result_fields_keys() ->
    lists:append(record_fields_keys('AT'),
        ea_aics_store_allocated_ancillaries:result_fields_keys()).

%% ===================================================================
%% Internal functions
%% ===================================================================

do_create_transaction(ConnectionPid, FlightId, AncillaryBookingInput) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                {ok, AncillaryBookingId} = do_create(ConnectionPid, FlightId,
                    AncillaryBookingInput),
                {ok, AncillaryBooking} = do_read(ConnectionPid, FlightId,
                    AncillaryBookingId),
                {atomic, {ok, AncillaryBooking}}
            end, self()),
    Response.

do_update_transaction(ConnectionPid, FlightId, AncillaryBookingId, AncillaryBookingUpdates) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                case do_update(ConnectionPid, FlightId,
                        AncillaryBookingId, AncillaryBookingUpdates) of
                    ok ->
                        {ok, AncillaryBooking} = do_read(ConnectionPid, FlightId,
                            AncillaryBookingId),
                        {atomic, {ok, AncillaryBooking}};
                    {error, not_found} ->
                        {atomic, {error, not_found}}
                end
            end, self()),
    Response.

do_create(ConnectionPid, FlightId, AncillaryBookingInput) ->
    AncillaryBookingId = ea_aics_store:generate_uuid(),
    RecordFieldsInputs = record_fields(AncillaryBookingId, FlightId,
        AncillaryBookingInput),
    Query = sqerl:sql({insert, 'ANCILLARY_TX', RecordFieldsInputs}, true),
    {updated, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{affectedrows = 1} = QueryResult,
    {ok, AncillaryBookingId}.

do_read(ConnectionPid, FlightId) ->
    ResultFieldsKeys = result_fields_keys(),
    Query = sqerl:sql({select, ResultFieldsKeys,
        {from, [{'ANCILLARY_TX', as, 'AT'},
                {'ANCILLARY_INVENTORY', as, 'AI'},
                {'ANCILLARY_MASTER', as, 'AM'}]},
        {where, {'and', [{{'AT', 'ANCILLARY_INVENTORY_UUID'}, '=', {'AI', 'UUID'}},
                         {{'AI', 'ANCILLARY_MASTER_UUID'}, '=', {'AM', 'UUID'}},
                         {{'AI', 'FLIGHT_UUID'}, '=', FlightId}]}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    AncillaryBookings = parse_query_result(QueryResult),
    {ok, AncillaryBookings}.

do_read(ConnectionPid, FlightId, AncillaryBookingId) ->
    ResultFieldsKeys = result_fields_keys(),
    Query = sqerl:sql({select, ResultFieldsKeys,
        {from, [{'ANCILLARY_TX', as, 'AT'},
                {'ANCILLARY_INVENTORY', as, 'AI'},
                {'ANCILLARY_MASTER', as, 'AM'}]},
        {where, {'and', [{{'AT', 'ANCILLARY_INVENTORY_UUID'}, '=', {'AI', 'UUID'}},
                         {{'AI', 'ANCILLARY_MASTER_UUID'}, '=', {'AM', 'UUID'}},
                         {{'AI', 'FLIGHT_UUID'}, '=', FlightId},
                         {{'AT', 'UUID'}, '=', AncillaryBookingId}]}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    case parse_query_result(QueryResult) of
        [AncillaryBooking] ->
            {ok, AncillaryBooking};
        [] ->
            {error, not_found}
    end.

do_update(ConnectionPid, _FlightId, AncillaryBookingId, _AncillaryBookingUpdates) ->
    Query = sqerl:sql({update, 'ANCILLARY_TX', [],
        {where, {'UUID', '=', AncillaryBookingId}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

do_delete(ConnectionPid, _FlightId, AncillaryBookingId) ->
    Query = sqerl:sql({delete, {from, 'ANCILLARY_TX'},
        {where, {'UUID', '=', AncillaryBookingId}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

parse_query_result(#mysql_result{rows = QueryResultRows} = _QueryResult) ->
    [parse_query_result_row(QueryResultRow) || QueryResultRow <- QueryResultRows].

parse_query_result_row(QueryResultRow) ->
    [AncillaryBookingId, _AllocatedAncillaryId, AncBook_CustomerId,
     AncBook_TransactionId, AncBook_OperationType, AncBook_BookingTime,
     AncBook_Quantity, AncBook_ModifiedTime
     | AllocatedAncillaryQueryResultRow] = QueryResultRow,
    AllocatedAncillary = ea_aics_store_allocated_ancillaries:parse_query_result_row(
        AllocatedAncillaryQueryResultRow),
    #ea_aics_ancillary_booking{id = AncillaryBookingId,
                               txn_id = AncBook_TransactionId,
                               customer_id = AncBook_CustomerId,
                               operation_type = AncBook_OperationType,
                               booking_time = AncBook_BookingTime,
                               quantity = AncBook_Quantity,
                               modified_time = AncBook_ModifiedTime,
                               allocated_ancillary = AllocatedAncillary}.

record_fields_keys() ->
    ['UUID', 'ANCILLARY_INVENTORY_UUID', 'CUSTOMER_UUID', 'ANC_TXN_ID',
        'OPERATION_TYPE', 'BOOKING_TIME', 'QUANTITY', 'MODIFIED_TIME'].

record_input_values(AncillaryBookingId, AncillaryBookingInput) ->
    AllocatedAncillaryId =
        record_input_value(AncillaryBookingInput#ea_aics_ancillary_booking.allocated_ancillary),
    AncBook_CustomerId =
        record_input_value(AncillaryBookingInput#ea_aics_ancillary_booking.customer_id),
    AncBook_TransactionId =
        record_input_value(AncillaryBookingInput#ea_aics_ancillary_booking.txn_id),
    AncBook_OperationType =
        record_input_value(AncillaryBookingInput#ea_aics_ancillary_booking.operation_type),
    AncBook_BookingTime =
        record_input_value(AncillaryBookingInput#ea_aics_ancillary_booking.booking_time),
    AncBook_Quantity =
        record_input_value(AncillaryBookingInput#ea_aics_ancillary_booking.quantity),
    [AncillaryBookingId, AllocatedAncillaryId, AncBook_CustomerId,
        AncBook_TransactionId, AncBook_OperationType, AncBook_BookingTime,
        AncBook_Quantity].

record_input_value(undefined) ->
    null;
record_input_value(Value) ->
    Value.

record_fields(AncillaryBookingId, _FlightId, AncillaryBookingInput) ->
    Fields = ['UUID', 'ANCILLARY_INVENTORY_UUID', 'CUSTOMER_UUID', 'ANC_TXN_ID',
        'OPERATION_TYPE', 'BOOKING_TIME', 'QUANTITY'],
    lists:zip(Fields, record_input_values(AncillaryBookingId, AncillaryBookingInput)).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

module_test_() ->

    ConnectionPid = self(),

    {foreach,
     fun()  ->
        ok = meck:new(mysql_conn, [non_strict])
     end,
     fun(_) ->
        ?assert(meck:validate(mysql_conn)),
        ok = meck:unload(mysql_conn)
     end,
     [
        {"create",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AllocatedAncillaryId = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    AncillaryBookingId = <<"0575d95b0beb444186cd41a555f43daa">>,
                    AncillaryBookingInput = ea_aics_store_test:ancillary_booking_input(AllocatedAncillaryId),

                    ok = meck:new(uuid, [non_strict, passthrough]),
                    ok = meck:expect(uuid, get_v4, [], AncillaryBookingId),
                    ok = meck:expect(uuid, uuid_to_string, ['_', '_'], AncillaryBookingId),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch({ok, AncillaryBookingId},
                        do_create(ConnectionPid, FlightId, AncillaryBookingInput)),

                    ok = meck:wait(uuid, get_v4, '_', 1000),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000),

                    ?assert(meck:validate(uuid)),
                    ok = meck:unload(uuid)

                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryId_1 = <<"0ea1f0e177fc4832a248371c3a884b4b">>,
                    AncillaryId_2 = <<"b5b89655228a4689bec675c5808f316d">>,
                    AllocatedAncillaryId_1 = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    AllocatedAncillaryId_2 = <<"0575d95b0beb444186cd41a555f43daa">>,
                    AncillaryBookingId_1 = <<"228bd21f759f454e84e63e88301cd4f3">>,
                    AncillaryBookingId_2 = <<"b5b89655228a4689bec675c5808f316d">>,

                    AncillaryBookingRows = ea_aics_store_test:ancillary_booking_rows(
                        [{AncillaryBookingId_1, FlightId, AllocatedAncillaryId_1, AncillaryId_1},
                         {AncillaryBookingId_2, FlightId, AllocatedAncillaryId_2, AncillaryId_2}]),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'],
                        {data, #mysql_result{rows = AncillaryBookingRows}}),

                    ?assertMatch({ok,
                        [#ea_aics_ancillary_booking{
                            id = AncillaryBookingId_1,
                            allocated_ancillary =
                                #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_1,
                                                             flight = #ea_aics_flight{id = FlightId},
                                                             ancillary = #ea_aics_ancillary{id = AncillaryId_1}}},
                         #ea_aics_ancillary_booking{
                            id = AncillaryBookingId_2,
                            allocated_ancillary =
                                #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_2,
                                                             flight = #ea_aics_flight{id = FlightId},
                                                             ancillary = #ea_aics_ancillary{id = AncillaryId_2}}}]},
                        do_read(ConnectionPid, FlightId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryId = <<"0ea1f0e177fc4832a248371c3a884b4b">>,
                    AllocatedAncillaryId = <<"228bd21f759f454e84e63e88301cd4f3">>,
                    AncillaryBookingId = <<"b5b89655228a4689bec675c5808f316d">>,

                    AncillaryBookingRows = ea_aics_store_test:ancillary_booking_rows(
                        [{AncillaryBookingId, FlightId, AllocatedAncillaryId, AncillaryId}]),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'],
                        {data, #mysql_result{rows = AncillaryBookingRows}}),

                    ?assertMatch({ok,
                        #ea_aics_ancillary_booking{id = AncillaryBookingId,
                                                    allocated_ancillary =
                                                        #ea_aics_allocated_ancillary{id = AllocatedAncillaryId,
                                                                                     flight = #ea_aics_flight{id = FlightId},
                                                                                     ancillary = #ea_aics_ancillary{id = AncillaryId}}}},
                        do_read(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryBookingId = <<"b5b89655228a4689bec675c5808f316d">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = []}}),

                    ?assertMatch({error, not_found}, do_read(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"update",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryBookingId = <<"b5b89655228a4689bec675c5808f316d">>,
                    AncillaryBookingUpdates = [],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_update(ConnectionPid, FlightId, AncillaryBookingId, AncillaryBookingUpdates)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryBookingId = <<"b5b89655228a4689bec675c5808f316d">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_delete(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryBookingId = <<"b5b89655228a4689bec675c5808f316d">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 0}}),

                    ?assertMatch({error, not_found}, do_delete(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        }
     ]
    }.

-endif.