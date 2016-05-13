-module(serdic_cluster_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    true = register_agent_proc(),
    ok   = cluster_mnesia:start(),
    serdic_cluster_sup:start_link().

stop(_State) ->
    ok.

-spec register_agent_proc() -> true.
register_agent_proc() ->
    true = erlang:register(serdic_cluster, erlang:self()).
