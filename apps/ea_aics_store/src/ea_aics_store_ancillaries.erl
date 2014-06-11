%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System
%%% ancillaries store interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_ancillaries).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create/0,
         read/0,
         read/1,
         update/2,
         delete/1]).

-export_type([]).

-include_lib("mysql/include/mysql.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Creates an ancillary in the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec create() -> {ok, #ea_aics_ancillary{}}.

create() ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_create_transaction(ConnectionPid)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillaries from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read() -> {ok, [#ea_aics_ancillary{}]}.

read() ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid)
        end).

%%------------------------------------------------------------------------------
%% @doc Reads the ancillary from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec read(binary()) -> {ok, #ea_aics_ancillary{}} | {error, not_found}.

read(AncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_read(ConnectionPid, AncillaryId)
        end).

%%------------------------------------------------------------------------------
%% @doc Updates the ancillary in the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec update(binary(), term()) -> {ok, #ea_aics_ancillary{}} | {error, not_found}.

update(AncillaryId, AncillaryUpdates) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_update_transaction(ConnectionPid, AncillaryId, AncillaryUpdates)
        end).

%%------------------------------------------------------------------------------
%% @doc Deletes the ancillary from the store.
%%
%% @end
%%------------------------------------------------------------------------------

-spec delete(binary()) -> ok | {error, not_found}.

delete(AncillaryId) ->
    ea_aics_store:do_query(
        fun(ConnectionPid) ->
            do_delete(ConnectionPid, AncillaryId)
        end).

%% ===================================================================
%% Internal functions
%% ===================================================================

do_create_transaction(ConnectionPid) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                {ok, AncillaryId} = do_create(ConnectionPid),
                {ok, _Ancillary} = do_read(ConnectionPid, AncillaryId)
            end, self()),
    Response.

do_update_transaction(ConnectionPid, AncillaryId, AncillaryUpdates) ->
    {atomic, Response} =
        mysql_conn:transaction(ConnectionPid,
            fun() ->
                case do_update(ConnectionPid, AncillaryId, AncillaryUpdates) of
                    ok ->
                        {ok, _Ancillary} = do_read(ConnectionPid, AncillaryId);
                    {error, not_found} ->
                        {error, not_found}
                end
            end, self()),
    Response.

do_create(ConnectionPid) ->
    AncillaryId = ea_aics_store:generate_uuid(),
    AncillaryMasterCode = 111,
    Query = sqerl:sql({insert, 'ANCILLARY_MASTER', [{'UUID', AncillaryId},
                                                    {'ANC_MASTER_CODE', AncillaryMasterCode}]}, true),
    {updated, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{affectedrows = 1} = QueryResult,
    {ok, AncillaryId}.

do_read(ConnectionPid) ->
    Query = sqerl:sql({select, ['UUID', 'ANC_MASTER_CODE'], {from, 'ANCILLARY_MASTER'}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    Ancillaries = parse_query_result(QueryResult),
    {ok, Ancillaries}.

do_read(ConnectionPid, AncillaryId) ->
    Query = sqerl:sql({select, ['UUID', 'ANC_MASTER_CODE'],
        {from, 'ANCILLARY_MASTER'}, {where, {'UUID', '=', AncillaryId}}}, true),
    {data, QueryResult} = ea_aics_store:do_fetch(ConnectionPid, Query),
    #mysql_result{fieldinfo = _Fields,
                  rows = _Rows} = QueryResult,
    case parse_query_result(QueryResult) of
        [Ancillary] ->
            {ok, Ancillary};
        [] ->
            {error, not_found}
    end.

do_update(ConnectionPid, AncillaryId, _AncillaryUpdates) ->
    Query = sqerl:sql({update, 'ANCILLARY_MASTER', [],
        {where, {'UUID', '=', AncillaryId}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

do_delete(ConnectionPid, AncillaryId) ->
    Query = sqerl:sql({delete, {from, 'ANCILLARY_MASTER'},
        {where, {'UUID', '=', AncillaryId}}}, true),
    case ea_aics_store:do_fetch(ConnectionPid, Query) of
        {updated, #mysql_result{affectedrows = 1}} ->
            ok;
        {updated, #mysql_result{affectedrows = 0}} ->
            {error, not_found}
    end.

parse_query_result(#mysql_result{rows = QueryResultRows} = _QueryResult) ->
    [parse_query_result_row(QueryResultRow) || QueryResultRow <- QueryResultRows].

parse_query_result_row(QueryResultRow) ->
    [AncillaryId, _AncillaryMasterCode] = QueryResultRow,
    #ea_aics_ancillary{id = AncillaryId}.

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
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

                    ok = meck:new(uuid, [non_strict, passthrough]),
                    ok = meck:expect(uuid, get_v4, [], AncillaryId),
                    ok = meck:expect(uuid, uuid_to_string, ['_', '_'], AncillaryId),

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch({ok, AncillaryId}, do_create(ConnectionPid)),

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
                    AncillaryId_1 = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryId_2 = <<"c25a06bb2764493d97fbecbda9300b67">>,
                    AncillaryMasterCode_1 = <<"111">>,
                    AncillaryMasterCode_2 = <<"222">>,
                    Ancillary_1 = #ea_aics_ancillary{id = AncillaryId_1},
                    Ancillary_2 = #ea_aics_ancillary{id = AncillaryId_2},
                    Ancillaries = [Ancillary_1, Ancillary_2],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = [[AncillaryId_1, AncillaryMasterCode_1],
                                                                                                      [AncillaryId_2, AncillaryMasterCode_2]]}}),

                    ?assertMatch({ok, Ancillaries}, do_read(ConnectionPid)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryMasterCode = <<"111">>,
                    Ancillary = #ea_aics_ancillary{id = AncillaryId},

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = [[AncillaryId, AncillaryMasterCode




                        ]]}}),

                    ?assertMatch({ok, Ancillary}, do_read(ConnectionPid, AncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"read",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {data, #mysql_result{rows = []}}),

                    ?assertMatch({error, not_found}, do_read(ConnectionPid, AncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"update",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,
                    AncillaryUpdates = [],

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_update(ConnectionPid, AncillaryId, AncillaryUpdates)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 1}}),

                    ?assertMatch(ok, do_delete(ConnectionPid, AncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        },
        {"delete",
            [
                fun() ->
                    AncillaryId = <<"4cbd913e6d5d449ea0e4b53606c01f1b">>,

                    ok = meck:expect(mysql_conn, fetch, ['_', '_', '_'], {updated, #mysql_result{affectedrows = 0}}),

                    ?assertMatch({error, not_found}, do_delete(ConnectionPid, AncillaryId)),

                    ok = meck:wait(mysql_conn, fetch, '_', 1000)
                end
            ]
        }
     ]
    }.

-endif.
