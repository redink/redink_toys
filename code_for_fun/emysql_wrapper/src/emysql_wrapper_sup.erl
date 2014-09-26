%%%-------------------------------------------------------------------
%%% @author redink <redink@ptthink.com>
%%% @copyright (C) 2013, redink
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2013 by redink <redink@ptthink.com>
%%%-------------------------------------------------------------------
-module(emysql_wrapper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, 
           [ 
            {emysql_wrapper_pool_sup, 
             {emysql_wrapper_pool_tmp_sup, start_link, [emysql_wrapper_pool_sup, emysql_wrapper_pool]}, 
             permanent, 5000, supervisor, [emysql_wrapper_pool_tmp_sup]}
           ]} }.

