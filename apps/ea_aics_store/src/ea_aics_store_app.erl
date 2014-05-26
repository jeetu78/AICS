%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System store interface app callback
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store_app).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

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
    {ok, PoolerSupervisorPid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
    PoolConfig = [{name, memsql},
                  {max_count, 1},
                  {init_count, 1},
                  {start_mfa,
                    {mysql_conn,
                     start_link,
                     ["127.0.0.1", 3306, "root", "", "AICS", fun(_, _, _, _) -> ok end, utf8, memsql]}}],
    {ok, _PoolPid} = pooler:new_pool(PoolConfig),
    {ok, PoolerSupervisorPid}.

stop(_State) ->
    ok.

%% ===================================================================
%%  supervisor callbacks
%% ===================================================================

init([]) ->
    PoolerSup = pooler_supervisor_child_spec(),
    {ok, {{one_for_one, 0, 1}, [PoolerSup]}}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

pooler_supervisor_child_spec() ->
    {pooler_sup, {pooler_sup, start_link, []},
        permanent, infinity, supervisor, [pooler_sup]}.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

sanity_test_() ->
    [
        ?_assertMatch(local, ?SCOPE)
    ].

callback_test_() ->

    PoolerSup = pooler_supervisor_child_spec(),

    [
        ?_assertMatch(ok, stop([])),
        ?_assertMatch({ok, Pid} when is_pid(Pid), start(temporary, [])),
        ?_assertMatch({ok, {{one_for_one, 0, 1}, [PoolerSup]}}, init([]))
    ].

-endif.
