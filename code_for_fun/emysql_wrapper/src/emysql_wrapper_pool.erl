%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(emysql_wrapper_pool).

-behaviour(gen_server).

-export([start_link/9]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {poolname,
                poolcount}).

start_link(PoolName, PoolCount, PoolNum, DBIP, DBPort,
           DBUser, DBPassword, DBDatabase, DBChst) ->
    gen_server:start_link({local, erlang:list_to_atom(PoolName)}, ?MODULE, 
                          [PoolName, PoolCount, PoolNum, DBIP, DBPort, 
                           DBUser, DBPassword, DBDatabase, DBChst],
                          []).

init([PoolName, PoolCount, PoolNum, DBIP, DBPort,
      DBUser, DBPassword, DBDatabase, DBChst]) ->
    erlang:process_flag(trap_exit, true),
    NodePoolFlag = init_pools(PoolName, PoolCount, PoolNum, DBIP, DBPort,
                              DBUser, DBPassword, DBDatabase, DBChst),
    case NodePoolFlag of
        true ->
            {ok, #state{poolname = PoolName, 
                        poolcount = PoolCount}};
        _ -> 
            {stop, error}
    end.

handle_call({querysql, SQL, TimeOut}, _, 
             #state{poolname = PoolName,
                    poolcount = PoolCount} = State) ->
    {reply, 
     emysql_wrapper_util:fetch_all(get_pool(PoolName, PoolCount), SQL, TimeOut),
     State};

handle_call(stop, _, State) ->
    delete_pool(State#state.poolname, State#state.poolcount),
    {stop, normal, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {norpely, State}.

terminate(_Reason, State) ->
    delete_pool(State#state.poolname, State#state.poolcount),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------

init_pools(_, 0, _PoolNum, _, _, _, _, _, _) ->
    true;

init_pools(PoolName, PoolCount, PoolNum, DBIP, DBPort,
           DBUser, DBPassword, DBDatabase, DBChst) ->
    Pool = erlang:list_to_atom(lists:append([PoolName, "_", erlang:integer_to_list(PoolCount)])),

    emysql_wrapper_util:unlinkmysql(Pool),

    case emysql_wrapper_util:linkmysql(Pool, PoolNum, DBIP, DBPort, 
                              DBUser, DBPassword,
                              DBDatabase, DBChst) of
        ok ->
            init_pools(PoolName, PoolCount - 1, PoolNum,  DBIP, DBPort,
                       DBUser, DBPassword, DBDatabase, DBChst);
        _ ->
            false
    end.

delete_pool(_, 0) ->
    ok;
delete_pool(PoolName, PoolCount) ->
    emysql_wrapper_util:unlinkmysql(
      erlang:list_to_atom(
        lists:append([PoolName, "_", erlang:integer_to_list(PoolCount)])
       )
     ).

get_pool(PoolName, PoolCount) ->
    erlang:list_to_atom(PoolName ++ "_" ++ erlang:integer_to_list(PoolCount)).