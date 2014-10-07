-module(waiting_queue_sup).

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
    WaitingQueueNormalSup = 
        {waiting_queue_normal_sup, 
             {waiting_queue_tmp_sup, start_link, [waiting_queue_normal_sup, waiting_queue_normal]}, 
             permanent, 5000, supervisor, [waiting_queue_normal_sup]},
    %%WaitingQueueXXXSup = 
    %%    {waiting_queue_XXX_sup, 
    %%         {waiting_queue_tmp_sup, start_link, [waiting_queue_XXX_sup, waiting_queue_meta]}, 
    %%         permanent, 5000, supervisor, [waiting_queue_XXX_sup]},
    {ok, { {one_for_one, 5, 10}, 
        [WaitingQueueNormalSup,
         %%WaitingQueueXXXSup,
         ?CHILD(waiting_queue_mgr, worker)]} }.

