-module(monmgr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = lager:start(),
    ok = serdic_cluster:start(),
    monmgr_sup:start_link().

stop(_State) ->
    ok.
