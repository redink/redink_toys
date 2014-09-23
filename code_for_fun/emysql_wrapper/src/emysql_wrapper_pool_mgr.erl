%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(emysql_wrapper_pool_mgr).

-behaviour(gen_server).

-include("emysql_wrapper.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([new_pool/1, drop_pool/1, info_pool/1]).

-define(SERVER, ?MODULE). 

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

new_pool(PoolMeta) ->
    gen_server:call(?SERVER, {new_pool, PoolMeta}).

drop_pool(PoolName) ->
    gen_server:call(?SERVER, {drop_pool, PoolName}).

info_pool(PoolName) ->
    case catch ets:lookup(pool_info, PoolName) of
        [] ->
            {error, noexit};
        [{PoolName, _, DBIP, DBport}] ->
            {PoolName, [DBIP, DBport]};
        _ ->
            {error, noexit}
    end.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ets:new(pool_info, [named_table]),
    {ok, #state{}}.

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
handle_call({new_pool, PoolMeta}, _, State) ->
    PoolName = get_keylist_val(<<"poolname">>, PoolMeta, ?MS_DB_POOL_NAME),
    DBIP   = get_keylist_val(<<"dbip">>, PoolMeta, ?MS_DB_IP),
    DBport = get_keylist_val(<<"dbport">>, PoolMeta, ?MS_DB_PORT),

    case ets:lookup(pool_info, PoolName) of
        [] ->
            A =
                supervisor:start_child(emysql_wrapper_pool_sup, 
                                       [PoolName, 1, 1,
                                        DBIP,
                                        DBport,
                                        get_keylist_val(<<"dbuser">>    , PoolMeta, ?MS_DB_USER    ),
                                        get_keylist_val(<<"dbpassword">>, PoolMeta, ?MS_DB_PASSWORD),
                                        get_keylist_val(<<"dbdatabase">>, PoolMeta, ?MS_DB_DATABASE),
                                        get_keylist_val(<<"dbchst">>    , PoolMeta, ?MS_DB_CHST    )
                                       ]),
            ets:insert(pool_info, {PoolName, A, DBIP, DBport}),
            {reply, A, State};
        [{PoolName, _, _, _}] ->
            {reply, repeat, State};
        _ ->
            {reply, error, State}
    end;

handle_call({drop_pool, {PoolName, DBIP, DBport}}, _, State) ->
    case catch ets:lookup(pool_info, PoolName) of
        [] ->
            {reply, ok, State};
        [{PoolName, _, DBIP, DBport}] ->
            {reply, ok, State};
        [{PoolName, _, _, _}] ->
            catch gen_server:call(erlang:list_to_atom(PoolName), stop),
            ets:delete(pool_info, PoolName),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;

handle_call({drop_pool, PoolName}, _, State) ->
    case catch ets:lookup(pool_info, PoolName) of
        [] ->
            {reply, ok, State};
        [{PoolName, _, _, _}] ->
            catch gen_server:stop(erlang:list_to_atom(PoolName), stop),
            ets:delete(pool_info, PoolName),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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

get_keylist_val(Key, KeyList, Default) ->
    case catch lists:keyfind(Key, 1, KeyList) of
        {Key, Value} ->
            Value;
        _ ->
            Default
    end.
