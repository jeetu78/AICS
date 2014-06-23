%% @author M1020387
%% @doc @todo Add description to 'ConnectionServer'.


-module(connectionServer).
-include_lib("ea_aics_store.hrl").
-behaviour(gen_server).


%% ====================================================================
%% API functions
%% ====================================================================
%% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).


-export([start_server/1,execute/1]).
%% ====================================================================
%% application callbacks
%% ====================================================================
%% start(_Type, _Args) ->
%% %%  io:format("Starting application\n"),
%% %%     start_server().
%% ok.
%% 
%% stop(_State) ->
%%     ok.

%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)

start_server(DBConfig) -> 
	io:format("Starting Server\n"),
	gen_server:start_link({local, ?MODULE},?MODULE, [DBConfig], []).

execute(Query) ->
	 gen_server:call(?MODULE,Query).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([#dbConfig{user=User,password=Password,host=Host,database=Database}]) ->
    process_flag(trap_exit, true),
    mysql:start_link(connPool, Host, User, Password, Database),
	{ok,[]}.


%% Synchronous, possible return values  
% {reply,Reply,NewState} 
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState} 
% {stop,Reason,NewState}
handle_call(Query, From, State) -> 
	io:format("~p~p",[From,State]),
	if 
		Query /= [] ->
			case mysql:fetch(connPool,Query) of
				{data, QueryResult} ->
					#mysql_result{rows = QueryResultRows}=QueryResult,
					{reply,{data,QueryResultRows},State};
				{updated, QueryResult} ->
					#mysql_result{affectedrows=Rows}=QueryResult,
					{reply,{no_of_rows,Rows},State};
				{error,QueryResult}->
					#mysql_result{error=Error}=QueryResult,
					{reply,{error,Error},State}
			end;
		true ->
			{reply,{error,"Invalid Query"},State}
	end.


%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
    {stop, normal, State};
%% generic async handler
handle_cast(Message, State) ->
	io:format("~p", [Message]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info(_Message, _Server) -> 
    {noreply, _Server}.

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}. 
