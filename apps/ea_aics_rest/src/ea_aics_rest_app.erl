%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface app callback
%%%
%%% @end
%%%=============================================================================

-module(ea_aics_rest_app).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

-export_type([]).

-define(SCOPE, local).

%% ===================================================================
%%  API
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------

%% ===================================================================
%%  application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
    {ok, Pid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
    ok = start_listeners(),
    {ok, Pid}.

stop(_State) ->
    ok.

%% ===================================================================
%%  supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 0, 1},[]}}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

start_listeners() ->
    GConfList = [],
    AICS_SConfList = [{appmods, []}],
    SConfList = [AICS_SConfList],
    {ok, SConf, GConf, ChildSpecs} = yaws_api:embedded_start_conf("/tmp", SConfList, GConfList),
    ok = lists:foreach(fun(ChildSpec) -> {ok, _Child} = supervisor:start_child(?MODULE, ChildSpec) end, ChildSpecs),
    ok = yaws_api:setconf(GConf, SConf).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).

sanity_test_() ->
    [
        ?_assertMatch(local, ?SCOPE)
    ].

callback_test_() ->
    [
        ?_assertMatch(ok, stop([])),
        ?_assertMatch({ok, Pid} when is_pid(Pid), start(temporary, [])),
        ?_assertMatch({ok, {{one_for_one, 0, 1}, []}}, init([]))
    ].

-endif.
