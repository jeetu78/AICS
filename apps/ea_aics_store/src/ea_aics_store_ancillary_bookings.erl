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
         delete/2]).

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

-spec create(binary(), binary()) -> {ok, #ea_aics_ancillary_booking{}}.

create(FlightId, AllocatedAncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_create_transaction(ConnectionPid, FlightId, AllocatedAncillaryId)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillary bookings from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read(_FlightId) -> {ok, [#ea_aics_ancillary_booking{}]}.

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
            do_update_transaction(ConnectionPid, FlightId, AncillaryBookingId, AncillaryBookingUpdates)
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

%% ===================================================================
%% Internal functions
%% ===================================================================

do_create_transaction(ConnectionPid, FlightId, AllocatedAncillaryId) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                {ok, AncillaryBookingId} = do_create(ConnectionPid, FlightId, AllocatedAncillaryId),
                {ok, AncillaryBooking} = do_read(ConnectionPid, FlightId, AncillaryBookingId),
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
                        {ok, AncillaryBooking} = do_read(ConnectionPid, FlightId, AncillaryBookingId),
                        {atomic, {ok, AncillaryBooking}};
                    {error, not_found} ->
                        {atomic, {error, not_found}}
                end
            end, self()),
    Response.

do_create(ConnectionPid, _FlightId, AllocatedAncillaryId) ->
    AncillaryBookingId = ea_aics_store:generate_uuid(),
    Query = sqerl:sql({insert, 'ANCILLARY_TX', [{'UUID', AncillaryBookingId},
                                                {'ANCILLARY_INVENTORY_UUID', AllocatedAncillaryId}]}, true),
    {updated, QueryResult} = mysql_conn:fetch(ConnectionPid, Query, self()),
    #mysql_result{affectedrows = 1} = QueryResult,
    {ok, AncillaryBookingId}.

do_read(ConnectionPid, FlightId) ->
    Query = sqerl:sql({select, [{{'AT', 'UUID'}, as, 'UUID'},
        {{'AT', 'ANCILLARY_INVENTORY_UUID'}, as, 'ANCILLARY_INVENTORY_UUID'}],
        {from, [{'ANCILLARY_TX', as, 'AT'}, {'ANCILLARY_INVENTORY', as, 'AI'}]},
        {where, {'and', [{{'AT', 'ANCILLARY_INVENTORY_UUID'}, '=', {'AI', 'UUID'}},
        {{'AI', 'FLIGHT_UID'}, '=', FlightId}]}}}, true),
    {data, QueryResult} = mysql_conn:fetch(ConnectionPid, Query, self()),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    AncillaryBookings = parse_query_result(QueryResult),
    {ok, AncillaryBookings}.

do_read(ConnectionPid, _FlightId, AncillaryBookingId) ->
    Query = sqerl:sql({select, ['UUID', 'ANCILLARY_INVENTORY_UUID'],
        {from, 'ANCILLARY_TX'}, {where, {'UUID', '=', AncillaryBookingId}}}, true),
    {data, QueryResult} = mysql_conn:fetch(ConnectionPid, Query, self()),
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
    case mysql_conn:fetch(ConnectionPid, Query, self()) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

do_delete(ConnectionPid, _FlightId, AncillaryBookingId) ->
    Query = sqerl:sql({delete, {from, 'ANCILLARY_TX'},
        {where, {'UUID', '=', AncillaryBookingId}}}, true),
    case mysql_conn:fetch(ConnectionPid, Query, self()) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

parse_query_result(#mysql_result{rows = QueryResultRows} = _QueryResult) ->
    [parse_query_result_row(QueryResultRow) || QueryResultRow <- QueryResultRows].

parse_query_result_row(QueryResultRow) ->
    [AncillaryBookingId, AllocatedAncillaryId] = QueryResultRow,
    AllocatedAncillary = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId},
    #ea_aics_ancillary_booking{id = AncillaryBookingId,
                               allocated_ancillary = AllocatedAncillary}.

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

                    ok = meck:new(uuid, [non_strict, passthrough]),
                    ok = meck:expect(uuid, get_v4, [], AncillaryBookingId),
                    ok = meck:expect(uuid, uuid_to_string, ['_', '_'], AncillaryBookingId),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch({ok, AncillaryBookingId}, do_create(ConnectionPid, FlightId, AllocatedAncillaryId)),

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
                    AllocatedAncillaryId_1 = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    AllocatedAncillaryId_2 = <<"0575d95b0beb444186cd41a555f43daa">>,
                    AllocatedAncillary_1 = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_1},
                    AllocatedAncillary_2 = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_2},
                    AncillaryBookingId_1 = <<"228bd21f759f454e84e63e88301cd4f3">>,
                    AncillaryBookingId_2 = <<"b5b89655228a4689bec675c5808f316d">>,
                    AncillaryBooking_1 = #ea_aics_ancillary_booking{id = AncillaryBookingId_1,
                                                                    allocated_ancillary = AllocatedAncillary_1},
                    AncillaryBooking_2 = #ea_aics_ancillary_booking{id = AncillaryBookingId_2,
                                                                    allocated_ancillary = AllocatedAncillary_2},
                    AncillaryBookings = [AncillaryBooking_1, AncillaryBooking_2],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'],
                        {data, #mysql_result{rows = [[AncillaryBookingId_1, AllocatedAncillaryId_1],
                                                     [AncillaryBookingId_2, AllocatedAncillaryId_2]]}}),

                    ?assertMatch({ok, AncillaryBookings}, do_read(ConnectionPid, FlightId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AllocatedAncillaryId = <<"228bd21f759f454e84e63e88301cd4f3">>,
                    AllocatedAncillary = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId},
                    AncillaryBookingId = <<"b5b89655228a4689bec675c5808f316d">>,
                    AncillaryBooking = #ea_aics_ancillary_booking{id = AncillaryBookingId,
                                                                  allocated_ancillary = AllocatedAncillary},

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'],
                        {data, #mysql_result{rows = [[AncillaryBookingId, AllocatedAncillaryId]]}}),

                    ?assertMatch({ok, AncillaryBooking}, do_read(ConnectionPid, FlightId, AncillaryBookingId)),

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