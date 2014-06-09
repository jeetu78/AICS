%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker controller
%%%
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

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a broker controller.
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
    NewState = lists:foldl(
        fun(_Seq, State_Acc) ->
            do_add_consumer(State_Acc)
        end, State, lists:seq(1, 5)),
    {noreply, NewState};
handle_info({'DOWN', ConsumerMonitorRef, _Type, ConsumerPid, _Info}, State) ->
    #state{consumers = Consumers} = State,
    case dict:find(ConsumerPid, Consumers) of
        {ok, ConsumerMonitorRef} ->
            timer:sleep(10000),
            NewState = do_add_consumer(State),
            {noreply, NewState};
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

%% ===================================================================
%%  Internal Functions
%% ===================================================================

do_add_consumer(State) ->
    #state{consumers = Consumers} = State,
    {ok, ConsumerPid} = ea_cs_mq_consumer_sup:new_broker_consumer(),
    ConsumerMonitorRef = erlang:monitor(process, ConsumerPid),
    State#state{consumers = dict:store(ConsumerPid, ConsumerMonitorRef, Consumers)}.