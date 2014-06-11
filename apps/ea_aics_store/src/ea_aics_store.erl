%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System store interface
%%% @end
%%%=============================================================================
-module(ea_aics_store).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_pool_member/0,
         do_query/1,
         generate_uuid/0]).

-export_type([]).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a member of the store pool. Called by the Pooler app.
%% @end
%%------------------------------------------------------------------------------
-spec start_pool_member() -> {ok, pid()}.

start_pool_member() ->
    {ok, Host} = application:get_env(ea_aics_store, host),
    {ok, Port} = application:get_env(ea_aics_store, port),
    {ok, User} = application:get_env(ea_aics_store, username),
    {ok, Pass} = application:get_env(ea_aics_store, password),
    {ok, DbName} = application:get_env(ea_aics_store, db_name),
    {ok, _ConnectionPid} = mysql_conn:start_link(Host, Port, User, Pass,
        DbName, fun(_, _, _, _) -> ok end, utf8).

%%------------------------------------------------------------------------------
%% @doc Executes a query on the storage system.
%% @end
%%------------------------------------------------------------------------------
-spec do_query(fun()) -> term().

do_query(OperationFun) ->
    ConnectionPid = pooler:take_member(memsql),
    Result = OperationFun(ConnectionPid),
    ok = pooler:return_member(memsql, ConnectionPid, ok),
    Result.

%%------------------------------------------------------------------------------
%% @doc Generates a UUID (Globally Unique Identifier)
%% @end
%%------------------------------------------------------------------------------
-spec generate_uuid() -> binary().

generate_uuid() ->
    uuid:get_v4().

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

-endif.
