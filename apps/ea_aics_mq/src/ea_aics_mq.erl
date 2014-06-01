%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System broker interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_mq).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([produce/1]).

-export_type([]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("ea_aics_core/include/ea_aics_core.hrl").

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Produces a message in the broker.
%%
%% @end
%%------------------------------------------------------------------------------

-spec produce(#ea_aics_ancillary_booking{}) -> ok.

produce(AncillaryBooking) ->
    BrokerConnectionPid = pooler:take_member(rabbitmq_connections),
    {ok, BrokerChannelPid} = amqp_connection:open_channel(BrokerConnectionPid),
    #'queue.declare_ok'{queue = <<"customer_scoring">>} =
        amqp_channel:call(BrokerChannelPid, #'queue.declare'{queue = <<"customer_scoring">>, durable = true}),
    #'confirm.select_ok'{} = amqp_channel:call(BrokerChannelPid, #'confirm.select'{}),
    PublishMethod = #'basic.publish'{exchange = <<>>, routing_key = <<"customer_scoring">>},
    MessageProperties = #'P_basic'{delivery_mode = 2},
    Message = #amqp_msg{props = MessageProperties, payload = term_to_binary(AncillaryBooking)},
    ok = amqp_channel:call(BrokerChannelPid, PublishMethod, Message),
    true = amqp_channel:wait_for_confirms_or_die(BrokerChannelPid, 1000),
    ok = amqp_channel:close(BrokerChannelPid),
    ok = pooler:return_member(rabbitmq_connections, BrokerChannelPid, ok),
    ok.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

-endif.
