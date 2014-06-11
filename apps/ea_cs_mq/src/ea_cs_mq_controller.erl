%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker controller
%%% @end
%%%=============================================================================
-module(ea_cs_mq_controller).

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

-record(state, {consumers = dict:new()}).

%%==============================================================================
%%  API
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a broker controller.
%% @end
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%=============================================================================
%%  gen_server callbacks
%%=============================================================================

init([]) ->
    {ok, #state{}, 0}.

handle_info(timeout, State) ->
    {ok, Consumers} = application:get_env(ea_cs_mq, num_consumers),
    NewState = lists:foldl(
        fun(_Seq, State_Acc) ->
            do_add_consumer(State_Acc)
        end, State, lists:seq(1, Consumers)),
    {noreply, NewState};
handle_info({'DOWN', ConsumerRef, _Type, ConsumerPid, _Info},
                #state{consumers = Consumers} = State) ->
    {ok, Interval} = application:get_env(ea_cs_mq, restart_interval),
    case dict:find(ConsumerPid, Consumers) of
        {ok, ConsumerRef} ->
            timer:sleep(Interval),
            {noreply, do_add_consumer(State)};
        _Other ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%=============================================================================
%%  Internal Functions
%%=============================================================================

do_add_consumer(#state{consumers = Consumers} = State) ->
    {ok, ConsumerPid} = ea_cs_mq_consumer_sup:new_broker_consumer(),
    ConsumerRef = erlang:monitor(process, ConsumerPid),
    State#state{consumers = dict:store(ConsumerPid, ConsumerRef, Consumers)}.
