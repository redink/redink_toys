%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) , redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink
%%%-------------------------------------------------------------------
-module(ptnature_mysql_mgr_last_{{new_ptnature_mysql_mgr_last_id}}).

-behaviour(gen_server).

%% API
-export([start_link/0,
         record_request/0,
         get_value/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIME_INTERVAL, 1000).

-define(HIBERNATE_TIMEOUT, hibernate).

-record(state, {queue,
                queue_len}).

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

record_request() ->
    LocalTime = calendar:local_time(),
    case catch ets:lookup(?MODULE, LocalTime) of
        [] ->
            ets:insert(?MODULE, {LocalTime, 1});
        _ ->
            ets:update_counter(?MODULE, LocalTime, 1)
    end.

get_value() ->
    lists:sum([X || {_, X} <- ets:tab2list(?MODULE)]).

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
    erlang:process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, public]),
    erlang:send(erlang:self(), {create_ets}),
    {ok, #state{queue = queue:new(),
                queue_len = {{new_ptnature_mysql_mgr_last_id}}*60}, ?HIBERNATE_TIMEOUT}.

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
handle_info({create_ets}, #state{queue = Queue, queue_len = QueueLen} = State) ->
    erlang:send_after(?TIME_INTERVAL, erlang:self(), {create_ets}),

    LocalTime = calendar:local_time(),
    case catch ets:lookup(?MODULE, LocalTime) of
        [] ->
            ets:insert(?MODULE, {LocalTime, 0});
        _ ->
            ok
    end,
 
    NewQueue = queue:in(LocalTime, Queue),
    case queue:len(NewQueue) > QueueLen of
        true ->
            erlang:send(erlang:self(), {dec_queue}),
            {noreply, State#state{queue = NewQueue}, ?HIBERNATE_TIMEOUT};
        _ ->
            {noreply, State#state{queue = NewQueue}, ?HIBERNATE_TIMEOUT}
    end;

handle_info({dec_queue}, #state{queue = Queue, queue_len = QueueLen} = State) ->
    case queue:len(Queue) > QueueLen of
        true ->
            {{value, OldLocalTime}, NewQueue} = queue:out(Queue),
            ets:delete(?MODULE, OldLocalTime),
            erlang:send(erlang:self(), {dec_queue}),
            {noreply, State#state{queue = NewQueue}, ?HIBERNATE_TIMEOUT};
        _ ->
            {noreply, State, ?HIBERNATE_TIMEOUT}
    end;
 
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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

