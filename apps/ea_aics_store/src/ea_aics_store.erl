%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System store interface
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_store).

-export([generate_uuid/0]).

-export_type([]).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Executes a query on the storage system.
%%
%% @end
%%------------------------------------------------------------------------------

%% -spec do_query(fun()) -> term().
%% 
%% do_query(OperationFun) ->
%%     OperationFun().
%%   

%%------------------------------------------------------------------------------
%% @doc Generates a UUID (Globally Unique Identifier)
%%
%% @end
%%------------------------------------------------------------------------------

-spec generate_uuid() -> binary().

generate_uuid() ->
    uuid:get_v4().
