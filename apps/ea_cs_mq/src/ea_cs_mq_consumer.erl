%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker consumer
%%%
%%% @end
%%%=============================================================================

-module(ea_cs_mq_consumer).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).
-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         terminate/2]).

-export([start_link/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {broker_channel_pid,
                broker_channel_monitor}).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a broker consumer.
%%
%% @end
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%%  gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}, 0}.

handle_info(timeout, State) ->
    BrokerConnectionPid = pooler:take_member(rabbitmq),
    {ok, BrokerChannelPid} = amqp_connection:open_channel(BrokerConnectionPid),
    BrokerChannelMonitorRef = erlang:monitor(process, BrokerChannelPid),
    ok = pooler:return_member(rabbitmq, BrokerConnectionPid, ok),
    #'queue.declare_ok'{queue = <<"customer_scoring">>} =
        amqp_channel:call(BrokerChannelPid, #'queue.declare'{queue = <<"customer_scoring">>, durable = true}),
    ConsumerMethod = #'basic.consume'{queue = <<"customer_scoring">>},
    #'basic.consume_ok'{consumer_tag = _Tag} = amqp_channel:subscribe(BrokerChannelPid, ConsumerMethod, self()),
    {noreply, State#state{broker_channel_pid = BrokerChannelPid,
                          broker_channel_monitor = BrokerChannelMonitorRef}};
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info(#'basic.cancel_ok'{}, State) ->
    {stop, normal, State};
handle_info({#'basic.deliver'{delivery_tag = Tag}, _Content}, State) ->
    %% TODO do some work here on message content
    #state{broker_channel_pid = BrokerChannelPid} = State,
    ok = amqp_channel:cast(BrokerChannelPid, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.