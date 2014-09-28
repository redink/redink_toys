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
         create_queue/4,
         create_normal_queue/3,
         create_normal_queue/2]).

-export([route_queue/3,
         route_queue/5]).

-export([get_queue_type/1]).

-export([get_queue_waiting/1,
         get_queue_waiting_size/1]).

-export([cancel_route/2]).


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
                                         [normal, MaxNum, TimeInterval]) of
        {ok, Pid} ->
            waiting_queue_mgr:created_queue(Pid, MaxNum, TimeInterval);
        _ ->
            ok
    end,
    Result.

create_normal_queue(ProcessName, MaxNum, TimeInterval) ->
    case Result = supervisor:start_child(waiting_queue_normal_sup, 
                                         [ProcessName, normal, MaxNum, TimeInterval]) of
        {ok, Pid} ->
            waiting_queue_mgr:created_queue(Pid, ProcessName, MaxNum, TimeInterval);
        _ ->
            ok
    end,
    Result.

route_queue(Queue, MsgID, Msg) ->
    gen_server:cast(Queue, {push, erlang:self(), MsgID, Msg}).

route_queue(Queue, MsgID, Mod, Fun, Args) ->
    gen_server:cast(Queue, {push, erlang:self(), MsgID, {Mod, Fun, Args}}).

get_queue_type(Queue) ->
    gen_server:call(Queue, {get_type}).

get_queue_waiting(Queue) ->
    gen_server:call(Queue, {get_waiting}).

get_queue_waiting_size(Queue) ->
    gen_server:call(Queue, {get_waiting_size}).

cancel_route(Queue, MsgID) ->
    gen_server:cast(Queue, {pull, MsgID}).

    