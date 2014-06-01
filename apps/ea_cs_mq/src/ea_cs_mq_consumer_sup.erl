%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker consumer supervisor
%%%
%%% @end
%%%=============================================================================

-module(ea_cs_mq_consumer_sup).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0,
         new_broker_consumer/0]).

-export_type([]).

-define(SCOPE, local).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a broker consumer supervisor.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.

start_link() ->
    {ok, SupPid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
    {ok, SupPid}.

%%------------------------------------------------------------------------------
%% @doc Starts a new broker consumer.
%%
%% @end
%%------------------------------------------------------------------------------

-spec new_broker_consumer() -> {ok, pid()}.

new_broker_consumer() ->
    BrokerConnectionPid = pooler:take_member(rabbitmq_connections),
    {ok, BrokerConsumerChildPid} = supervisor:start_child(?MODULE, [BrokerConnectionPid]),
    ok = pooler:return_member(rabbitmq_connections, BrokerConnectionPid, ok),
    {ok, BrokerConsumerChildPid}.

%% ===================================================================
%%  supervisor callbacks
%% ===================================================================

init([]) ->
    BrokerConsumerChildSpec = broker_consumer_child_spec(),
    {ok, {{simple_one_for_one, 10, 100}, [BrokerConsumerChildSpec]}}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

broker_consumer_child_spec() ->
    {ea_cs_mq_consumer, {ea_cs_mq_consumer, start_link, []},
        permanent, 5000, worker, [ea_cs_mq_consumer]}.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

sanity_test_() ->
    [
        ?_assertMatch(local, ?SCOPE)
    ].

callback_test_() ->

    BrokerConsumerChildSpec = broker_consumer_child_spec(),

    [
        ?_assertMatch({ok, {{simple_one_for_one, 10, 100}, [BrokerConsumerChildSpec]}}, init([]))
    ].

-endif.