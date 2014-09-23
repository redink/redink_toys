%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2014 by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(waiting_queue_normal).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(HIBERNATE_TIMEOUT, 10000).

-record(state, {max_num = 10,
                waiting_queue = queue:new(),
                time_interval = 60000}).

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
start_link(MaxNum, TimeInterval) ->
    gen_server:start_link(?MODULE, [MaxNum, TimeInterval], []).

start_link(ProcessName, MaxNum, TimeInterval) ->
    gen_server:start_link({local, ProcessName}, ?MODULE, [MaxNum, TimeInterval], []).

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
init([MaxNum, TimeInterval]) ->
    {ok, #state{max_num = MaxNum, time_interval = TimeInterval}, ?HIBERNATE_TIMEOUT}.

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
handle_cast({push, OriginPid, Msg}, #state{max_num = 0, 
                                           waiting_queue = WaitingQueue} = State) ->
    erlang:monitor(process, OriginPid),
    NewState = State#state{waiting_queue = queue:in({OriginPid, Msg}, WaitingQueue)},
    {noreply, NewState, ?HIBERNATE_TIMEOUT};

handle_cast({push, OriginPid, Msg}, #state{max_num = MaxNum, 
                                            time_interval = TimeInterval} = State) ->
    
    %% do something operation
    queue_handle(OriginPid, Msg),

    %% send after 60s, tell the queue seed will active
    erlang:send_after(TimeInterval, erlang:self(), {active}),
    NewState = State#state{max_num = MaxNum - 1},
    {noreply, NewState, ?HIBERNATE_TIMEOUT};

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
handle_info({active}, #state{max_num = MaxNum,
                             time_interval = TimeInterval,
                             waiting_queue = WaitingQueue} = State) ->
    case queue:out(WaitingQueue) of
        {{value, {OriginPid, Msg}}, NewWaitingQueue} ->
            case erlang:is_process_alive(OriginPid) of
                true ->
                    %% do something operations
                    queue_handle(OriginPid, Msg),

                    erlang:send_after(TimeInterval, erlang:self(), {active}),
                    
                    {noreply, State#state{max_num = MaxNum,
                                          waiting_queue = NewWaitingQueue}, ?HIBERNATE_TIMEOUT};
                _ ->
                    {noreply, State#state{max_num = MaxNum + 1,
                                          waiting_queue = NewWaitingQueue}, ?HIBERNATE_TIMEOUT}
            end;
        {empty, NewWaitingQueue} ->
            {noreply, State#state{max_num = MaxNum + 1,
                                  waiting_queue = NewWaitingQueue}, ?HIBERNATE_TIMEOUT}
    end;

handle_info({'DOWN', MonitorRef, process, OriginPid, _Reason}, 
            #state{waiting_queue = WaitingQueue} = State) ->
    erlang:demonitor(MonitorRef, [flush]),
    NewWaitingQueue = queue:filter(fun(X) -> 
                                        erlang:element(1, X) =/= OriginPid 
                                   end, 
                                  WaitingQueue),
    {noreply, State#state{waiting_queue = NewWaitingQueue}, ?HIBERNATE_TIMEOUT};

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

queue_handle(OriginPid, Msg) ->
    io:format("------------origin pid  ~p, msg ~p ~n", [OriginPid, Msg]).

