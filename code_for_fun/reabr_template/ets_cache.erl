%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created : by redink ets_cache's template
%%%-------------------------------------------------------------------
-module({{ets_cache_id}}).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([update_query_result/4
         , update_query_result/5
         , update_waiting_list/5
         , get_cache_info/4
         , clean_cache/1
         , get_queue_len/1
         , reset_queue_len/2
         , delete_cache/4
         , stop_cache/1

         , reset_queue_mem/2
         , get_cache_info/1
         , get_cache_default_info/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MIN_CACHE_LEN, 500).
-define(MAX_CACHE_LEN, 10000).
-define(MIN_CACHE_MEM, 5).
-define(MAX_CACHE_MEM, 500).

-define(HIBERNATE_TIMEOUT, 30000).

-record(state, {etstable,
                queue,
                queue_len,
                queue_mem}).

%%%===================================================================
%%% API
%%%===================================================================

stop_cache(Sid) ->
    gen_server:call(term_to_existing_atom(Sid), {stop_cache}).

get_queue_len(Sid) ->
    case erlang:whereis(term_to_existing_atom(Sid)) of
        undefined ->
            {error, noproc};
        _ ->
            gen_server:call(term_to_existing_atom(Sid), {get_queue_len})
    end.

reset_queue_len(Sid, QueueLen) ->
    case erlang:whereis(term_to_existing_atom(Sid)) of
        undefined ->
            {error, noproc};
        _ ->
            gen_server:call(term_to_existing_atom(Sid), {reset_queue_len, QueueLen})
    end.

reset_queue_mem(Sid, Mem) ->
    case erlang:whereis(term_to_existing_atom(Sid)) of
        undefined ->
            {error, noproc};
        _ ->
            gen_server:call(term_to_existing_atom(Sid), {reset_queue_mem, Mem})
    end.

clean_cache(Sid) when erlang:is_binary(Sid) ->
    clean_cache(erlang:binary_to_list(Sid));
clean_cache(Sid) when erlang:is_list(Sid) ->
    clean_cache(erlang:list_to_existing_atom(Sid));
clean_cache(Sid) when erlang:is_atom(Sid) ->
    case erlang:whereis(Sid) of
        undefined ->
            ok;
        _ ->
            gen_server:call(Sid, {clean_cache})
    end;
clean_cache(Pid) when erlang:is_pid(Pid) ->
    gen_server:call(Pid, {clean_cache}).

get_cache_info(Sid) when erlang:is_binary(Sid) ->
    get_cache_info(erlang:binary_to_list(Sid));
get_cache_info(Sid) when erlang:is_list(Sid) ->
    get_cache_info(erlang:list_to_existing_atom(Sid));
get_cache_info(Sid) when erlang:is_atom(Sid) ->
    case erlang:whereis(Sid) of
        undefined ->
            {error, noproc};
        _ ->
            EtsName = erlang:list_to_existing_atom("cache_manager_" ++ erlang:atom_to_list(Sid)),
            {ets:info(EtsName, size), ets:info(EtsName, memory)*8/1024/1024}
    end.

get_cache_default_info(Sid) when erlang:is_binary(Sid) ->
    get_cache_default_info(erlang:binary_to_list(Sid));
get_cache_default_info(Sid) when erlang:is_list(Sid) ->
    get_cache_default_info(erlang:list_to_existing_atom(Sid));
get_cache_default_info(Sid) when erlang:is_atom(Sid) ->
    case erlang:whereis(Sid) of
        undefined ->
            {error, noproc};
        _ ->
            gen_server:call(Sid, {get_cache_default_info})
    end.

get_cache_info(get_cache_info, Sid, EtsName, ConditionList) ->
    gen_server:call(term_to_existing_atom(Sid),
                    {get_cache_info, EtsName, ConditionList}).

update_query_result(handling, Sid, EtsName, ConditionList) ->
    gen_server:call(term_to_existing_atom(Sid),
                    {handling, EtsName, ConditionList}).

update_query_result(handled, Sid, EtsName, ConditionList, QueryResult)->
    gen_server:call(term_to_existing_atom(Sid),
                    {handled, EtsName, ConditionList, QueryResult}).

update_waiting_list(waiting, Sid, EtsName, ConditionList, Pid)->
    gen_server:call(term_to_existing_atom(Sid),
                    {waiting, EtsName, ConditionList, Pid}).

delete_cache(delete_cache, Sid, EtsName, ConditionList) ->
    gen_server:call(term_to_existing_atom(Sid),
                    {delete_cache, EtsName, ConditionList}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(Sid, EtsName) ->
    gen_server:start_link({local, term_to_atom(Sid)}, ?MODULE, [EtsName], []).

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
init([EtsName]) ->
    EtsTable = ets:new(EtsName, [named_table,
                                 {read_concurrency, true},
                                 {write_concurrency, true}]),
    Queue = queue:new(),
    {ok, #state{etstable = EtsTable,
                queue = Queue,
                queue_len = 1000,
                queue_mem = 100}, ?HIBERNATE_TIMEOUT}.

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

handle_call({stop_cache}, _, State) ->
    {stop, normal, State};

handle_call({delete_cache, EtsName, ConditionList}, _, State) ->
    {reply, ets:delete(EtsName, ConditionList), State, ?HIBERNATE_TIMEOUT};

handle_call({get_cache_info, EtsName, ConditionList}, _, State) ->
    {reply, ets:lookup(EtsName, ConditionList), State, ?HIBERNATE_TIMEOUT};

handle_call({handling, EtsName, ConditionList}, _, State) ->
    ets:insert(EtsName, {ConditionList, <<"handling">>, [], []}),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

handle_call({handled, EtsName, ConditionList, QueryResult}, _, State) ->
    case ets:lookup(EtsName, ConditionList) of
        [] ->
            ets:insert(EtsName, {ConditionList, <<"handled">>, [], QueryResult}),
            erlang:send(erlang:self(), {refresh_queue, ConditionList}),
            erlang:send(erlang:self(), {refresh_mem});

        [{ConditionList, <<"handling">>, [], _}] ->
            ets:insert(EtsName, {ConditionList, <<"handled">>, [], QueryResult}),
            erlang:send(erlang:self(), {refresh_queue, ConditionList}),
            erlang:send(erlang:self(), {refresh_mem});

        [{ConditionList, <<"handling">>, WaitingList, _}] ->
            ets:insert(EtsName, {ConditionList, <<"handled">>, [], QueryResult}),
            erlang:send(erlang:self(), {refresh_queue, ConditionList}),
            erlang:send(erlang:self(), {refresh_mem}),
            F = fun() ->
                        [erlang:send(Pid, QueryResult) || Pid <- WaitingList]
                end,
            proc_lib:spawn(F);
        _ ->
            ok
    end,
    {reply, ok, State, ?HIBERNATE_TIMEOUT};


handle_call({waiting, EtsName, ConditionList, Pid}, _, State) ->
    case ets:lookup(EtsName, ConditionList) of
        [] ->
            ok;
        [{ConditionList, <<"handled">>, _, QueryResult}] ->
            erlang:send(Pid, QueryResult);
        [{ConditionList, <<"handling">>, A, B}] ->
            ets:insert(EtsName, {ConditionList, <<"handling">>, [Pid | A], B});
        _ ->
            ok
    end,            
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

handle_call({clean_cache}, _,
            #state{queue = Queue, etstable = EtsTable} = State) ->
    
    case queue:is_empty(Queue) of
        true ->
            ets:delete_all_objects(EtsTable),
            {reply, ok, State, ?HIBERNATE_TIMEOUT};
        _ ->

            NewQueue = drop_all_queue(Queue, queue:is_empty(Queue)),
            ets:delete_all_objects(EtsTable),

            {reply, ok, State#state{queue = NewQueue}, ?HIBERNATE_TIMEOUT}
    end;

handle_call({get_queue_len}, _,
            #state{queue_len = QueueLen} = State) ->
    {reply, {queue_len, QueueLen}, State, ?HIBERNATE_TIMEOUT};

handle_call({reset_queue_len, QueueLen}, _,
            #state{queue_len = OldQueueLen,
                   etstable = EtsTable,
                   queue = Queue} = State) ->
    if
        QueueLen =:= OldQueueLen ->
            {reply, {ok, QueueLen}, State, ?HIBERNATE_TIMEOUT};
        QueueLen < ?MIN_CACHE_LEN ->
            {reply, {too_small, QueueLen}, State, ?HIBERNATE_TIMEOUT};
        QueueLen > ?MAX_CACHE_LEN ->
            {reply, {too_large, QueueLen}, State, ?HIBERNATE_TIMEOUT};
        QueueLen >= ?MIN_CACHE_LEN andalso QueueLen < OldQueueLen ->
            {_, NewQueue} = reset_queue_len_do(QueueLen, queue:len(Queue), EtsTable, Queue),
            {reply, {ok, QueueLen}, State#state{queue_len = QueueLen,
                                                queue = NewQueue}, ?HIBERNATE_TIMEOUT};
        QueueLen > OldQueueLen andalso QueueLen =< ?MAX_CACHE_LEN ->
            {reply, {ok, QueueLen}, State#state{queue_len = QueueLen}, ?HIBERNATE_TIMEOUT};
        true ->
            {reply, ok, State, ?HIBERNATE_TIMEOUT}
    end;

handle_call({reset_queue_mem, Mem}, _,
            #state{queue_mem = QueueMem} = State) ->
    if
        Mem =:= QueueMem ->
            {reply, {ok, QueueMem}, State, ?HIBERNATE_TIMEOUT};
        Mem < ?MIN_CACHE_MEM ->
            {reply, {too_small, Mem}, State, ?HIBERNATE_TIMEOUT};
        Mem > ?MAX_CACHE_MEM ->
            {reply, {too_large, Mem}, State, ?HIBERNATE_TIMEOUT};
        Mem >= ?MIN_CACHE_MEM andalso Mem < QueueMem ->
            erlang:send(erlang:self(), {refresh_mem}),
            {reply, {ok, Mem}, State#state{queue_mem = Mem}, ?HIBERNATE_TIMEOUT};
        Mem > QueueMem andalso Mem =< ?MAX_CACHE_MEM ->
            {reply, {ok, Mem}, State#state{queue_mem = Mem}, ?HIBERNATE_TIMEOUT};
        true ->
            {reply, ok, State, ?HIBERNATE_TIMEOUT} 
    end;

handle_call({get_cache_default_info}, _,
            #state{queue_len = QueueLen, queue_mem = QueueMem} = State) ->
    {reply, {QueueLen, QueueMem}, State, ?HIBERNATE_TIMEOUT};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

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

handle_info({refresh_mem}, #state{queue_mem = UNQueueMem,
                                  queue = Queue,
                                  etstable = EtsTable} = State) ->
    QueueMem = UNQueueMem * 1024 * 1024 / 8,
    case catch ets:info(EtsTable, memory) of
        Mem when erlang:is_integer(Mem) ->
            if
                Mem > QueueMem ->

                    lager:warning("-@@@@@ ets table mem large than limit"),

                    { {value, OldConditionList}, NewQueue} = queue:out(Queue),
                    delete_old_ets(EtsTable, OldConditionList),

                    erlang:send(erlang:self(), {refresh_mem}),

                    {noreply, State#state{queue = NewQueue}, ?HIBERNATE_TIMEOUT};
                true ->
                    {noreply, State, ?HIBERNATE_TIMEOUT}
            end
            ;
        _ ->
            {noreply, State, ?HIBERNATE_TIMEOUT}
    end;


handle_info({refresh_queue, ConditionList},
            #state{queue = Queue,
                   queue_len = QueueLen,
                   etstable = EtsTable} = State) ->

    NewQueue = queue:in(ConditionList, Queue),

    case queue:len(NewQueue) > QueueLen of
        true ->

            lager:debug("queue length is large than queuelen"),

            { {value, OldConditionList}, NewQueue1} = queue:out(NewQueue),
            delete_old_ets(EtsTable, OldConditionList),

            NewQueueLen = dynamic_inc_queue(QueueLen),

            {noreply, State#state{queue = NewQueue1,
                                  queue_len = NewQueueLen}, 
             ?HIBERNATE_TIMEOUT};
        _ ->
            {noreply, State#state{queue = NewQueue,
                                  queue_len = QueueLen},
                      ?HIBERNATE_TIMEOUT}
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
dynamic_inc_queue(QueueLen) ->
    if
        QueueLen >= ?MAX_CACHE_LEN ->
            ?MAX_CACHE_LEN;
        QueueLen * 2 >= ?MAX_CACHE_LEN ->
            ?MAX_CACHE_LEN;
        true ->
            QueueLen * 2
    end.

reset_queue_len_do(QueueLen, NowQueueLen, EtsTable, Queue) ->
    if
        QueueLen >= NowQueueLen ->
            {QueueLen, Queue};
        true ->
            reset_queue_len_do_(QueueLen, NowQueueLen, EtsTable, Queue)
    end.

reset_queue_len_do_(QueueLen, QueueLen, _, Queue) ->
    {QueueLen, Queue};
reset_queue_len_do_(QueueLen, OldQueueLen, EtsTable, Queue) ->
    { {value, ConditionList}, NewQueue} = queue:out(Queue),
    delete_old_ets(EtsTable, ConditionList),
    reset_queue_len_do_(QueueLen, OldQueueLen - 1, EtsTable, NewQueue).

delete_old_ets(EtsTable, ConditionList) ->
    ets:delete(EtsTable, ConditionList).

drop_all_queue(Queue, true) ->
    Queue;
drop_all_queue(Queue, _Empty) ->
    NewQueue = queue:drop(Queue),
    drop_all_queue(NewQueue, queue:is_empty(NewQueue)).

term_to_existing_atom(Term) when erlang:is_binary(Term) ->
    term_to_existing_atom(erlang:binary_to_list(Term));

term_to_existing_atom(Term) when erlang:is_list(Term) ->
    term_to_existing_atom(erlang:list_to_existing_atom(Term));

term_to_existing_atom(Term) when erlang:is_atom(Term) ->
    Term.

term_to_atom(Term) when erlang:is_binary(Term) ->
    term_to_atom(erlang:binary_to_list(Term));

term_to_atom(Term) when erlang:is_list(Term) ->
    term_to_atom(erlang:list_to_atom(Term));

term_to_atom(Term) when erlang:is_atom(Term) ->
    Term.
