%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System
%%% allocated ancillaries store interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_allocated_ancillaries).

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
%% @doc Creates an allocated ancillary in the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec create(binary(), binary()) -> {ok, #ea_aics_allocated_ancillary{}}.

create(FlightId, AncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_create_transaction(ConnectionPid, FlightId, AncillaryId)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the allocated ancillaries from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read(binary()) -> {ok, [#ea_aics_allocated_ancillary{}]}.

read(FlightId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid, FlightId)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the allocated ancillary from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read(binary(), binary()) -> {ok, #ea_aics_allocated_ancillary{}} | {error, not_found}.

read(FlightId, AllocatedAncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid, FlightId, AllocatedAncillaryId)
        end).

%%------------------------------------------------------------------------------
%% @doc Updates the allocated ancillary in the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec update(binary(), binary(), term()) -> {ok, #ea_aics_allocated_ancillary{}} | {error, not_found}.

update(FlightId, AllocatedAncillaryId, AllocatedAncillaryUpdates) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_update_transaction(ConnectionPid, FlightId, AllocatedAncillaryId, AllocatedAncillaryUpdates)
        end).

%%------------------------------------------------------------------------------
%% @doc Deletes the allocated ancillary from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete(binary(), binary()) -> ok | {error, not_found}.

delete(FlightId, AllocatedAncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_delete(ConnectionPid, FlightId, AllocatedAncillaryId)
        end).

%% ===================================================================
%% Internal functions
%% ===================================================================

do_create_transaction(ConnectionPid, FlightId, AncillaryId) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                {ok, AllocatedAncillaryId} = do_create(ConnectionPid, FlightId, AncillaryId),
                {ok, AllocatedAncillary} = do_read(ConnectionPid, FlightId, AllocatedAncillaryId),
                {atomic, {ok, AllocatedAncillary}}
            end, self()),
    Response.

do_update_transaction(ConnectionPid, FlightId, AllocatedAncillaryId, AllocatedAncillaryUpdates) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                case do_update(ConnectionPid, FlightId,
                        AllocatedAncillaryId, AllocatedAncillaryUpdates) of
                    ok ->
                        {ok, AllocatedAncillary} = do_read(ConnectionPid, FlightId, AllocatedAncillaryId),
                        {atomic, {ok, AllocatedAncillary}};
                    {error, not_found} ->
                        {atomic, {error, not_found}}
                end
            end, self()),
    Response.

do_create(ConnectionPid, FlightId, AncillaryId) ->
    AllocatedAncillaryId = ea_aics_store:generate_uuid(),
    Query = sqerl:sql({insert, 'ANCILLARY_INVENTORY', [{'UUID', AllocatedAncillaryId},
                                                       {'FLIGHT_UUID', FlightId},
                                                       {'ANCILLARY_MASTER_UUID', AncillaryId}]}, true),
    {updated, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{affectedrows = 1} = QueryResult,
    {ok, AllocatedAncillaryId}.

do_read(ConnectionPid, FlightId) ->
    Query = sqerl:sql({select, ['UUID', 'FLIGHT_UUID', 'ANCILLARY_MASTER_UUID'],
        {from, 'ANCILLARY_INVENTORY'}, {where, {'FLIGHT_UUID', '=', FlightId}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    AllocatedAncillaries = parse_query_result(QueryResult),
    {ok, AllocatedAncillaries}.

do_read(ConnectionPid, FlightId, AllocatedAncillaryId) ->
    Query = sqerl:sql({select, ['UUID', 'FLIGHT_UUID', 'ANCILLARY_MASTER_UUID'],
        {from, 'ANCILLARY_INVENTORY'}, {where, {'and', [{'FLIGHT_UUID', '=', FlightId},
        {'UUID', '=', AllocatedAncillaryId}]}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    case parse_query_result(QueryResult) of
        [AllocatedAncillary] ->
            {ok, AllocatedAncillary};
        [] ->
            {error, not_found}
    end.

do_update(ConnectionPid, FlightId, AllocatedAncillaryId, _AllocatedAncillaryUpdates) ->
    Query = sqerl:sql({update, 'ANCILLARY_INVENTORY', [],
        {where, {'and', [{'FLIGHT_UUID', '=', FlightId},
        {'UUID', '=', AllocatedAncillaryId}]}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

do_delete(ConnectionPid, FlightId, AllocatedAncillaryId) ->
    Query = sqerl:sql({delete, {from, 'ANCILLARY_INVENTORY'},
        {where, {'and', [{'FLIGHT_UUID', '=', FlightId},
        {'UUID', '=', AllocatedAncillaryId}]}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

parse_query_result(#mysql_result{rows = QueryResultRows} = _QueryResult) ->
    [parse_query_result_row(QueryResultRow) || QueryResultRow <- QueryResultRows].

parse_query_result_row(QueryResultRow) ->
    [AllocatedAncillaryId, FlightId, AncillaryId] = QueryResultRow,
    Flight = #ea_aics_flight{id = FlightId},
    Ancillary = #ea_aics_ancillary{id = AncillaryId},
    #ea_aics_allocated_ancillary{id = AllocatedAncillaryId,
                                 flight = Flight,
                                 ancillary = Ancillary}.

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
                    AncillaryId = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    AllocatedAncillaryId = <<"0575d95b0beb444186cd41a555f43daa">>,

                    ok = meck:new(uuid, [non_strict, passthrough]),
                    ok = meck:expect(uuid, get_v4, [], AllocatedAncillaryId),
                    ok = meck:expect(uuid, uuid_to_string, ['_', '_'], AllocatedAncillaryId),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch({ok, AllocatedAncillaryId}, do_create(ConnectionPid, FlightId, AncillaryId)),

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
                    Flight = #ea_aics_flight{id = FlightId},
                    AncillaryId_1 = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    AncillaryId_2 = <<"0575d95b0beb444186cd41a555f43daa">>,
                    Ancillary_1 = #ea_aics_ancillary{id = AncillaryId_1},
                    Ancillary_2 = #ea_aics_ancillary{id = AncillaryId_2},
                    AllocatedAncillaryId_1 = <<"228bd21f759f454e84e63e88301cd4f3">>,
                    AllocatedAncillaryId_2 = <<"b5b89655228a4689bec675c5808f316d">>,
                    AllocatedAncillary_1 = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_1,
                                                                        flight = Flight,
                                                                        ancillary = Ancillary_1},
                    AllocatedAncillary_2 = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId_2,
                                                                        flight = Flight,
                                                                        ancillary = Ancillary_2},
                    AllocatedAncillaries = [AllocatedAncillary_1, AllocatedAncillary_2],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'],
                        {data, #mysql_result{rows = [[AllocatedAncillaryId_1, FlightId, AncillaryId_1],
                                                     [AllocatedAncillaryId_2, FlightId, AncillaryId_2]]}}),

                    ?assertMatch({ok, AllocatedAncillaries}, do_read(ConnectionPid, FlightId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    Flight = #ea_aics_flight{id = FlightId},
                    AncillaryId = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    Ancillary = #ea_aics_ancillary{id = AncillaryId},
                    AllocatedAncillaryId = <<"b5b89655228a4689bec675c5808f316d">>,
                    AllocatedAncillary = #ea_aics_allocated_ancillary{id = AllocatedAncillaryId,
                                                                      flight = Flight,
                                                                      ancillary = Ancillary},

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'],
                        {data, #mysql_result{rows = [[AllocatedAncillaryId, FlightId, AncillaryId]]}}),

                    ?assertMatch({ok, AllocatedAncillary}, do_read(ConnectionPid, FlightId, AllocatedAncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AllocatedAncillaryId = <<"b5b89655228a4689bec675c5808f316d">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = []}}),

                    ?assertMatch({error, not_found}, do_read(ConnectionPid, FlightId, AllocatedAncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"update",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AllocatedAncillaryId = <<"b5b89655228a4689bec675c5808f316d">>,
                    AllocatedAncillaryUpdates = [],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_update(ConnectionPid, FlightId, AllocatedAncillaryId, AllocatedAncillaryUpdates)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AllocatedAncillaryId = <<"b5b89655228a4689bec675c5808f316d">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_delete(ConnectionPid, FlightId, AllocatedAncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    FlightId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AllocatedAncillaryId = <<"b5b89655228a4689bec675c5808f316d">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 0}}),

                    ?assertMatch({error, not_found}, do_delete(ConnectionPid, FlightId, AllocatedAncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        }
     ]
    }.

-endif.
