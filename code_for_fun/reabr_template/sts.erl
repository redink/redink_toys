%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) , redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink
%%%-------------------------------------------------------------------
-module({{sts_id}}).

-behaviour(gen_server).

%% API
-export([start_link/0, now/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HIBERNATE_TIMEOUT, 10000).
-define(ETS_OPTS, [named_table, set, protected, {read_concurrency, true}]).
-define(UPDATE_TIME_INTERVAL, 500).

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

now() ->
    case catch ets:lookup(?MODULE, now) of
        [{now, Now}] ->
            Now;
        _ ->
            real_now()
    end.

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
    erlang:process_flag(priority, high),
    ets:new(?MODULE, ?ETS_OPTS),
    loop_for_update_time(),
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
handle_info(update_time, State) ->
    loop_for_update_time(),
    {noreply, State, ?HIBERNATE_TIMEOUT};

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

real_now() ->
    {T1, T2, _} = os:timestamp(),
    T1*1000000 + T2.

loop_for_update_time() ->
    ets:insert(?MODULE, {now, real_now()}),
    erlang:send_after(?UPDATE_TIME_INTERVAL, erlang:self(), update_time).
