%%%-------------------------------------------------------------------
%%% @author redink <cnredink@gmail.com>
%%% @copyright (C) 2014, redink
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2014 by redink <cnredink@gmail.com>
%%%-------------------------------------------------------------------

-module(waiting_queue).

-export([create_queue/3,
         create_queue/4]).

-export([route_queue/2]).


create_queue(normal, MaxNum, TimeInterval) ->
    create_normal_queue(MaxNum, TimeInterval);

create_queue(_Type, _MaxNum, _TimeInterval) ->
    unsupported.

create_queue(normal, ProcessName, MaxNum, TimeInterval) ->
    create_normal_queue(ProcessName, MaxNum, TimeInterval);

create_queue(_Type, _PorcessName, _MaxNum, _TimeInterval) ->
    unsupported.


create_normal_queue(MaxNum, TimeInterval) ->
    case Result = supervisor:start_child(waiting_queue_normal_sup, 
                                         [MaxNum, TimeInterval]) of
        {ok, Pid} ->
            waiting_queue_mgr:created_queue(Pid, MaxNum, TimeInterval);
        _ ->
            ok
    end,
    Result.

create_normal_queue(ProcessName, MaxNum, TimeInterval) ->
    case Result = supervisor:start_child(waiting_queue_normal_sup, 
                                         [ProcessName, MaxNum, TimeInterval]) of
        {ok, Pid} ->
            waiting_queue_mgr:created_queue(Pid, ProcessName, MaxNum, TimeInterval);
        _ ->
            ok
    end,
    Result.

route_queue(Queue, Msg) ->
    gen_server:cast(Queue, {push, erlang:self(), Msg}).
    