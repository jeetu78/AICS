%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Customer Scoring broker interface
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
%% @doc Starts a member of the broker pool. This is called by the Pooler.
%% @end
%%------------------------------------------------------------------------------
-spec start_pool_member() -> {ok, pid()}.

start_pool_member() ->
    {ok, User} = application:get_env(ea_cs_mq, username),
    {ok, Pass} = application:get_env(ea_cs_mq, password),
    {ok, Host} = application:get_env(ea_cs_mq, host),
    {ok, Port} = application:get_env(ea_cs_mq, port),
    % We are just not using the IP and Port variables now.
    Config = #amqp_params_network{username = list_to_binary(User),
                                  password = list_to_binary(Pass),
                                  host = Host,
                                  port = Port},
    {ok, _Conn} = amqp_connection:start(Config).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

-endif.
