%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2014 by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(waiting_queue_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([created_queue/3]).
-export([created_queue/4]).

-define(SERVER, ?MODULE).

-define(HIBERNATE_TIMEOUT, 10000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

created_queue(QueuePid, MaxNum, TimeInterval) ->
    gen_server:call(?MODULE, {created_queue, undefined, QueuePid, MaxNum, TimeInterval}).

created_queue(QueuePid, ProcessName, MaxNum, TimeInterval) ->
    gen_server:call(?MODULE, {created_queue, ProcessName, QueuePid, MaxNum, TimeInterval}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(queue_info, [named_table]),
    {ok, #state{}, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({created_queue, ProcessName, QueuePid, MaxNum, TimeInterval}, _From, State) ->
    ets:insert(queue_info, {ProcessName, QueuePid, MaxNum, TimeInterval}),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};


handle_call(_Request, _From, State) ->
    {reply, ok, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
               [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State, ?HIBERNATE_TIMEOUT}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

