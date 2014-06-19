%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System store interface app callback
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_app).
-include_lib("ea_aics_store.hrl").

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).
-export([getDBConfig/0]).
-export_type([]).

-define(SCOPE, local).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------

%% ===================================================================
%%  application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
    {ok, SupervisorPid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
%%     PoolConfig = [{name, memsql},
%%                   {max_count, 1},
%%                   {init_count, 1},
%%                   {start_mfa,
%%                     {mysql_conn,
%%                      start_link,
%%                      ["127.0.0.1", 3306, "root", "", "AICS", fun(_, _, _, _) -> ok end, utf8, memsql]}}],
%%     {ok, _PoolPid} = pooler:new_pool(PoolConfig),
    {ok, SupervisorPid}.

stop(_State) ->
    ok.

%% ===================================================================
%%  supervisor callbacks
%% ===================================================================

init([]) ->
    GenServerWorker = pooler_supervisor_child_spec(),
    {ok, {{one_for_one, 0, 1}, [GenServerWorker]}}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

pooler_supervisor_child_spec() ->
	DBConfig=getDBConfig(),
    {conn_sup, {connectionServer, start_server, [DBConfig]},
        permanent, 5000, worker, [connectionServer]}.

getDBConfig() ->
	#dbConfig{host="192.168.1.23",port=3306,database="AICS_NEW",user="root",password=""}.
