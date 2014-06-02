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

-export([]).

-export_type([]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

-endif.
