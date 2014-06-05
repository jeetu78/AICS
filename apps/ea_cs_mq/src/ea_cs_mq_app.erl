%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker interface app callback
%%%
%%% @end
%%%=============================================================================

-module(ea_cs_mq_app).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

-export_type([]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(SCOPE, local).

%% ===================================================================
%%  API
%% ===================================================================

%% ===================================================================
%%  application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
    {ok, AppSupervisorPid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
    {ok, AppSupervisorPid}.

stop(_State) ->
    ok.

%% ===================================================================
%%  supervisor callbacks
%% ===================================================================

init([]) ->
    SupChildSpecs = supervisor_child_specs(),
    {ok, {{one_for_one, 0, 1}, SupChildSpecs}}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

supervisor_child_specs() ->
    lists:flatten([pooler_supervisor_child_spec(),
                   broker_component_supervisor_child_spec()]).

pooler_supervisor_child_spec() ->
    pooler_supervisor_child_spec(erlang:whereis(pooler_sup)).

pooler_supervisor_child_spec(PoolerSup) when is_pid(PoolerSup) ->
    [];
pooler_supervisor_child_spec(undefined) ->
    [{pooler_sup, {pooler_sup, start_link, []},
        permanent, infinity, supervisor, [pooler_sup]}].

broker_component_supervisor_child_spec() ->
    [{ea_cs_mq_sup, {ea_cs_mq_sup, start_link, []},
        permanent, infinity, supervisor, [ea_cs_mq_sup]}].

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

sanity_test_() ->
    [
        ?_assertMatch(local, ?SCOPE)
    ].

app_callback_test_() ->
    [
        ?_assertMatch({ok, Pid} when is_pid(Pid), start(temporary, [])),
        ?_assertMatch(ok, stop([]))
    ].

sup_callback_test_() ->

    ChildSpecs = supervisor_child_specs(),

    [
        ?_assertMatch({ok, {{one_for_one, 0, 1}, ChildSpecs}}, init([]))
    ].

-endif.