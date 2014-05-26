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
    mysql_conn:transaction(ConnectionPid,
        fun() ->
            {ok, AncillaryBookingId} = do_create(ConnectionPid, FlightId, AllocatedAncillaryId),
            {ok, AncillaryBooking} = do_read(ConnectionPid, FlightId, AncillaryBookingId),
            {atomic, {ok, AncillaryBooking}}
        end, self()).

do_update_transaction(ConnectionPid, FlightId, AncillaryBookingId, AncillaryBookingUpdates) ->
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
        end, self()).

do_create(ConnectionPid, _FlightId, AllocatedAncillaryId) ->
    AncillaryBookingId = ea_aics_store:generate_uuid(),
    Query = <<(<<"INSERT INTO ANCILLARY_TX (ANCILLARY_TX_UUID, ANCILLARY_INVENTORY_UUID)
                VALUES ('">>)/binary,
              AncillaryBookingId/binary,
              (<<"','">>)/binary,
              AllocatedAncillaryId/binary,
              (<<"')">>)/binary>>,
    {updated, QueryResult} = mysql_conn:fetch(ConnectionPid, Query, self()),
    #mysql_result{affectedrows = 1} = QueryResult,
    {ok, AncillaryBookingId}.

do_read(ConnectionPid, FlightId) ->
    Query = <<(<<"SELECT ANCILLARY_TX.ANCILLARY_TX_UUID, ANCILLARY_TX.ANCILLARY_INVENTORY_UUID
                FROM ANCILLARY_TX INNER JOIN ANCILLARY_INVENTORY WHERE
                ANCILLARY_TX.ANCILLARY_INVENTORY_UUID=ANCILLARY_INVENTORY.ANCILLARY_INVENTORY_UUID
                AND ANCILLARY_INVENTORY.FLIGHT_UUID='">>)/binary,
              FlightId/binary,
              (<<"'">>)/binary>>,
    {data, QueryResult} = mysql_conn:fetch(ConnectionPid, Query, self()),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    AncillaryBookings = parse_query_result(QueryResult),
    {ok, AncillaryBookings}.

do_read(ConnectionPid, _FlightId, AncillaryBookingId) ->
    Query = <<(<<"SELECT ANCILLARY_TX_UUID, ANCILLARY_INVENTORY_UUID
                FROM ANCILLARY_TX WHERE
                ANCILLARY_TX_UUID='">>)/binary,
              AncillaryBookingId/binary,
              (<<"'">>)/binary>>,
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
    Query = <<(<<"UPDATE ANCILLARY_TX SET .. WHERE
                ANCILLARY_TX_UUID='">>)/binary,
              AncillaryBookingId/binary,
              (<<"'">>)/binary>>,
    case mysql_conn:fetch(ConnectionPid, Query, self()) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

do_delete(ConnectionPid, _FlightId, AncillaryBookingId) ->
    Query = <<(<<"DELETE FROM ANCILLARY_TX WHERE
                ANCILLARY_INVENTORY_UUID='">>)/binary,
              AncillaryBookingId/binary,
              (<<"'">>)/binary>>,
    case mysql_conn:fetch(ConnectionPid, Query, self()) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

parse_query_result(#mysql_result{rows = QueryResultRows} = _QueryResult) ->
    [parse_query_result_row(QueryResultRow) || QueryResultRow <- QueryResultRows].

parse_query_result_row(QueryResultRow) ->
    [AncillaryBookingId, AllocatedAncillaryId, AncillaryId] = QueryResultRow,
    Ancillary = #ea_aics_ancillary{id = AncillaryId},
    AllocatedAncillary = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId,
                                                      ancillary = Ancillary},
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
                    FlightId = <<"111">>,
                    AllocatedAncillaryId = <<"111">>,
                    AncillaryBookingId = <<"111">>,

                    ok = meck:new(uuid, [non_strict]),
                    ok = meck:expect(uuid, get_v4, [], AncillaryBookingId),

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
                    FlightId = <<"111">>,
                    AncillaryId_1 = <<"111">>,
                    AncillaryId_2 = <<"222">>,
                    Ancillary_1 = #ea_aics_ancillary{id = AncillaryId_1},
                    Ancillary_2 = #ea_aics_ancillary{id = AncillaryId_2},
                    AllocatedAncillaryId_1 = <<"111">>,
                    AllocatedAncillaryId_2 = <<"222">>,
                    AllocatedAncillary_1 = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_1,
                                                                        ancillary = Ancillary_1},
                    AllocatedAncillary_2 = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_2,
                                                                        ancillary = Ancillary_2},
                    AncillaryBookingId_1 = <<"111">>,
                    AncillaryBookingId_2 = <<"222">>,
                    AncillaryBooking_1 = #ea_aics_ancillary_booking{id = AncillaryBookingId_1,
                                                                    allocated_ancillary = AllocatedAncillary_1},
                    AncillaryBooking_2 = #ea_aics_ancillary_booking{id = AncillaryBookingId_2,
                                                                    allocated_ancillary = AllocatedAncillary_2},
                    AncillaryBookings = [AncillaryBooking_1, AncillaryBooking_2],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = [[AncillaryBookingId_1, AllocatedAncillaryId_1, AncillaryId_1],
                                                                                                      [AncillaryBookingId_2, AllocatedAncillaryId_2, AncillaryId_2]]}}),

                    ?assertMatch({ok, AncillaryBookings}, do_read(ConnectionPid, FlightId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"111">>,
                    AncillaryId = <<"111">>,
                    Ancillary = #ea_aics_ancillary{id = AncillaryId},
                    AllocatedAncillaryId = <<"111">>,
                    AllocatedAncillary = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId,
                                                                      ancillary = Ancillary},
                    AncillaryBookingId = <<"111">>,
                    AncillaryBooking = #ea_aics_ancillary_booking{id = AncillaryBookingId,
                                                                  allocated_ancillary = AllocatedAncillary},

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = [[AncillaryBookingId, AllocatedAncillaryId, AncillaryId]]}}),

                    ?assertMatch({ok, AncillaryBooking}, do_read(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"111">>,
                    AncillaryBookingId = <<"111">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = []}}),

                    ?assertMatch({error, not_found}, do_read(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"update",
            [
                fun() ->
                    FlightId = <<"111">>,
                    AncillaryBookingId = <<"111">>,
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
                    FlightId = <<"111">>,
                    AncillaryBookingId = <<"111">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_delete(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    FlightId = <<"111">>,
                    AncillaryBookingId = <<"111">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 0}}),

                    ?assertMatch({error, not_found}, do_delete(ConnectionPid, FlightId, AncillaryBookingId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        }
     ]
    }.

-endif.