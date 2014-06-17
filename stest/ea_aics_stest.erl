-module(ea_aics_stest).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

connect_client(Host) ->
    case lhttpc:connect_client(Host, [{use_cookies, true}]) of
        {error, econnrefused} ->
            connect_client(Host);
        {ok, Client} ->
            {ok, Client}
    end.

wait_alive(RemoteNode, RemoteNodeCookie) ->
    CurrentCookie = erlang:get_cookie(),
    true = erlang:set_cookie(node(), RemoteNodeCookie),
    ok = wait_alive(RemoteNode),
    true = erlang:set_cookie(node(), CurrentCookie).


wait_dead(RemoteNode, RemoteNodeCookie) ->
    CurrentCookie = erlang:get_cookie(),
    true = erlang:set_cookie(node(), RemoteNodeCookie),
    ok = wait_dead(RemoteNode),
    true = erlang:set_cookie(node(), CurrentCookie).

wait_alive(RemoteNode) ->
    case net_adm:ping(RemoteNode) of
        'pang' -> wait_alive(RemoteNode);
        'pong' -> ok
    end.

wait_dead(RemoteNode) ->
    case net_adm:ping(RemoteNode) of
        'pong' -> wait_dead(RemoteNode);
        'pang' -> ok
    end.

status({Status, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    Headers.

data_owner() ->
    receive
        stop -> exit(normal);
        _ -> data_owner()
    end.

data_insert(Table, Key, Value) ->
    true = ets:insert(Table, {Key, Value}).

data_lookup(Table, Key) ->
    [{Key, Value}] = ets:lookup(Table, Key),
    Value.
