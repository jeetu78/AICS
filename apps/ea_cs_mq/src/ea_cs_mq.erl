%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker interface
%%%
%%% @end
%%%=============================================================================

-module(ea_cs_mq).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_pool_member/0]).

-export_type([]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts a member of the broker pool.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_pool_member() -> {ok, pid()}.

start_pool_member() ->
    ConnectionConfig = #amqp_params_network{username = <<"ea">>,
                                            password = <<"ea">>},
    {ok, ConnectionPid} = amqp_connection:start(ConnectionConfig),
    {ok, ConnectionPid}.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

-endif.
