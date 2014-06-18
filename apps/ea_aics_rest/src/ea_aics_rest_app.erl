%%%=============================================================================
%%% @author Alexej Tessaro <alexej.tessaro@erlang-solutions.com>
%%% @doc The Ancillary Inventory Control System rest interface app callback
%%% @end
%%%=============================================================================
-module(ea_aics_rest_app).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ea_aics_rest_metrics.hrl").

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

-export_type([]).

-define(SCOPE, local).

%%=============================================================================
%%  API
%%=============================================================================

%%=============================================================================
%%  application callbacks
%%=============================================================================

start(_Type, _StartArgs) ->
    {ok, Pid} = supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []),
    ok = init_rest_metrics(),
    ok = start_listeners(),
    {ok, Pid}.

stop(_State) ->
    ok.

%%=============================================================================
%%  supervisor callbacks
%%=============================================================================

init([]) ->
    {ok, {{one_for_one, 0, 1},[]}}.

%%=============================================================================
%%  Internal Functions
%%=============================================================================

start_listeners() ->
    {ok, HostString} = application:get_env(ea_aics_rest, ip),
    % we want the IP in the config file to be a string.
    {ok, Host} = inet_parse:address(HostString),
    {ok, Port} = application:get_env(ea_aics_rest, port),
    GConfList = [],
    AICS_SConfList = [
        {listen, Host},
        {port, Port},
        {appmods, [
            {"/", ea_aics_rest}]}
        ],
    SConfList = [AICS_SConfList],
    {ok, SConf, GConf, ChildSpecs} = yaws_api:embedded_start_conf(
            "/tmp", SConfList, GConfList),
    ok = lists:foreach(
        fun(ChildSpec) ->
    %% TODO change to {ok, _Child} = supervisor:start_child(?MODULE, ChildSpec)
    %% no matching right now for a bug in the SENDFILE driver in
    %% yaws-1.98 on OSX Mavericks
            supervisor:start_child(?MODULE, ChildSpec)
        end, ChildSpecs),
    ok = yaws_api:setconf(GConf, SConf).

init_rest_metrics() ->
    ok = lists:foreach(fun(Name) -> exometer:new(Name, counter) end, counters()).

counters() ->
    [?C_TOT_REQ, ?C_OK_REQ, ?C_NOK_REQ, ?C_INVALID_REQ].

%%=============================================================================
%%  Tests
%%=============================================================================

-ifdef(TEST).

sanity_test_() ->
    [
        ?_assertMatch(local, ?SCOPE)
    ].

callback_test_() ->
    [
        ?_assertMatch(ok, stop([])),
        ?_assertMatch({ok, Pid} when is_pid(Pid),
                      begin
                        ok = application:load(ea_aics_rest),
                        [ok = application:start(App, temporary) || App <- applications()],
                        start(temporary, [])
                      end),
        ?_assertMatch({ok, {{one_for_one, 0, 1}, []}}, init([]))
    ].

applications() ->
    [compiler, syntax_tools, goldrush, lager, exometer].

-endif.
