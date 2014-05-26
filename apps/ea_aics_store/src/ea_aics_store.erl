%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System store interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([do_query/1,
         generate_uuid/0]).

-export_type([]).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Executes a query on the storage system.
%%
%% @end
%%------------------------------------------------------------------------------

-spec do_query(fun()) -> term().

do_query(OperationFun) ->
    ConnectionPid = pooler:take_member(memsql),
    Result = OperationFun(ConnectionPid),
    ok = pooler:return_member(memsql, ConnectionPid, ok),
    Result.

%%------------------------------------------------------------------------------
%% @doc Generates a UUID (Globally Unique Identifier)
%%
%% @end
%%------------------------------------------------------------------------------

-spec generate_uuid() -> binary().

generate_uuid() ->
    uuid:get_v4().

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

-endif.
