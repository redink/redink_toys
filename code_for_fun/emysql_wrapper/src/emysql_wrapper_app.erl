%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(emysql_wrapper_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    ok = ensure_application_start(crypto),
    ok = ensure_application_start(emysql),
    emysql_wrapper_sup:start_link().

stop(_State) ->
    ok.

ensure_application_start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        _Any ->
            error
    end.
