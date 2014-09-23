%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------
-module(emysql_wrapper_util).

-export([now_time/0,
	    get_node_pools/1]).

get_node_pools(PoolName) ->
    case ets:lookup(pool_info, PoolName) of
        [] ->
            error;
        [{PoolName, PoolList, _, _}] ->
            lists:nth(get_random(erlang:length(PoolList)), PoolList)
    end.

get_random(Num) ->
    {Res, _} = random:uniform_s(Num, erlang:now()),
    Res.

%%=========================================================================

now_time() ->
  {X, Y, _} = now(),
  X * 1000000 + Y.
