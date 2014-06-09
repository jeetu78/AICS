%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker consumer component supervisor
%%%
%%% @end
%%%=============================================================================

-module(ea_cs_mq_sup).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).

-export_type([]).

-define(SCOPE, local).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a broker consumer component supervisor.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.

start_link() ->
    {ok, SupPid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
    {ok, SupPid}.

%% ===================================================================
%%  supervisor callbacks
%% ===================================================================

init([]) ->
    SupChildSpecs = supervisor_child_specs(),
    {ok, {{one_for_all, 10, 100}, SupChildSpecs}}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

supervisor_child_specs() ->
    lists:flatten([broker_consumer_supervisor_child_spec(),
                   broker_controller_child_spec()]).

broker_consumer_supervisor_child_spec() ->
    [{ea_cs_mq_consumer_sup, {ea_cs_mq_consumer_sup, start_link, []},
        permanent, infinity, supervisor, [ea_cs_mq_consumer_sup]}].

broker_controller_child_spec() ->
    [{ea_cs_mq_controller, {ea_cs_mq_controller, start_link, []},
        permanent, 5000, worker, [ea_cs_mq_controller]}].

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

sanity_test_() ->
    [
        ?_assertMatch(local, ?SCOPE)
    ].

callback_test_() ->

    ChildSpecs = supervisor_child_specs(),

    [
        ?_assertMatch({ok, {{one_for_all, 10, 100}, ChildSpecs}}, init([]))
    ].

-endif.