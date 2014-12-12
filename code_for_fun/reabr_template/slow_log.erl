%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created : by redink slow_log's template
%%%-------------------------------------------------------------------
-module({{slowlog_id}}).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([record_slow_log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(ENSURE_ENABLED, ensure_enabled).
-define(SLOWLOG_TABLE, slowlog_table).

-define(HIBERNATE_TIMEOUT, 30000).
-define(SLOWLOG_MAX_LEN, 2000).
-define(FILE_MAX_LEN, 5).

-define(SLOWTIME_MAX, 150000000).
-define(SLOWTIME_MIN, 10000).

-record(state, {max_len_queue,
                file_queue,
                file_name,
                file_handler}).

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

record_slow_log(ConditionList, QueryTime) ->
    
    UUID = "create-uuid",
    LocalTime = calendar:local_time(),

    SystemInfo = get_system_info(),
    ets:insert(?SLOWLOG_TABLE, {UUID, 
                                LocalTime,
                                ConditionList,
                                QueryTime,
                                SystemInfo}),
    gen_server:cast(?MODULE, {refresh_queue, UUID}),
    gen_server:cast(?MODULE, {write_file_log, UUID, LocalTime, 
                              ConditionList, QueryTime, SystemInfo}).


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
    ets:new(?ENSURE_ENABLED, [named_table]),
    ets:new(?SLOWLOG_TABLE, [public, named_table]),
    MaxLenQueue = queue:new(),
    FileQueue   = queue:new(),
    {ok, #state{max_len_queue = MaxLenQueue,
                file_queue = FileQueue}, ?HIBERNATE_TIMEOUT}.

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
handle_cast({refresh_queue, UUID}, #state{max_len_queue = MaxLenQueue} = State) ->
    NewMaxLenQueue = queue:in(UUID, MaxLenQueue),
    case queue:len(MaxLenQueue) > ?SLOWLOG_MAX_LEN of
        true ->

            { {value, OldUUID}, NewMaxLenQueue1} = queue:out(NewMaxLenQueue),

            delete_old_ets(?SLOWLOG_TABLE, OldUUID),

            {noreply, State#state{max_len_queue = NewMaxLenQueue1},
             ?HIBERNATE_TIMEOUT};
        _ ->
            {noreply, State#state{max_len_queue = NewMaxLenQueue},
             ?HIBERNATE_TIMEOUT}
    end;

handle_cast({write_file_log, _UUID, { {Y, M, D}, _} = LocalTime, 
             ConditionList, QueryTime, SystemInfo},
             #state{file_name = OldFileName,
                    file_handler = OldFileHandler} = State) ->
    FileName = get_slow_log_file_name(Y, M, D),
    case FileName =:= OldFileName andalso exists(FileName) of
        true ->
            io:format(OldFileHandler,
                      "local time : ~p~nconditionlist : ~p~nquery time : ~p~nsystem info : ~p~n",
                      [LocalTime, ConditionList, QueryTime, SystemInfo]),
            io:format(OldFileHandler, "~p~n", ["#######################################################"]),
            erlang:send(erlang:self(), {refresh_file_queue, FileName}),
            {noreply, State, ?HIBERNATE_TIMEOUT};
        _ ->
            {ok, FileHandler} = file:open(FileName, [append]),
            io:format(FileHandler,
                      "local time : ~p~nconditionlist : ~p~nquery time : ~p~nsystem info : ~p~n",
                      [LocalTime, ConditionList, QueryTime, SystemInfo]),
            io:format(FileHandler, "~p~n", ["#######################################################"]),
            erlang:send(erlang:self(), {refresh_file_queue, FileName}),
            {noreply, State#state{file_name = FileName,
                                  file_handler = FileHandler},
                      ?HIBERNATE_TIMEOUT}
    end;

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

handle_info({refresh_file_queue, FileName},
             #state{file_queue = FileQueue} = State) ->
    case queue:member(FileName, FileQueue) of
        true ->
            {noreply, State, ?HIBERNATE_TIMEOUT};
        _ ->
            NewFileQueue = queue:in(FileName, FileQueue),

            case queue:len(NewFileQueue) > ?FILE_MAX_LEN of
                true ->

                    { {value, OldFileName}, NewFileQueue1} = queue:out(NewFileQueue),

                    delete_old_file_name(OldFileName),

                    {noreply, State#state{file_queue = NewFileQueue1}};
                _ ->
                    {noreply, State#state{file_queue = NewFileQueue}}
            end
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

get_system_info() ->
    [].

delete_old_ets(EtsTable, Key) ->
    ets:delete(EtsTable, Key).

get_slow_log_file_name(Y, M, D) ->
    "./log/slow_log." ++ 
        erlang:integer_to_list(Y) ++
        "_" ++ 
        erlang:integer_to_list(M) ++
        "_" ++
        erlang:integer_to_list(D).

delete_old_file_name(FileName) ->
    file:delete(FileName).

exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _} ->
            true;
        {error, _Reason} ->
            false
    end.
