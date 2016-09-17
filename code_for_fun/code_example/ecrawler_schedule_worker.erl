%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) , redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink
%%%-------------------------------------------------------------------
-module(ecrawler_schedule_worker).

-behaviour(gen_server).

-include("table_define.hrl").

%% API
-export([ start_link/3
        , stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HIBERNATE_TIMEOUT, hibernate).
-define(MNESIA     , ecrawler_mnesia).
-define(CONTASKNUM , 32).

-ifdef(TEST).
-define(SINGLEETSTABLELIMIT, 2).
-else.
-define(SINGLEETSTABLELIMIT, 200000).
-endif.


%%%===================================================================
%%% API
%%%===================================================================

-spec stop(pid()) -> ok.
stop(WorkerPid) ->
    erlang:send(WorkerPid, stop),
    ok.

start_link(TaskOwner, StrCid, MaterielTable) ->
    gen_server:start_link(?MODULE, [TaskOwner, StrCid, MaterielTable], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([TaskOwner, StrCid, MaterielTable]) ->
    _ = erlang:process_flag(trap_exit, true),
    erlang:send(self(), start_crawl_work),
    {ok, #{ strcid         => StrCid
          , taskowner      => TaskOwner
          , materiel_table => MaterielTable
          , contasknum     => 0
          , max_depth      => 100
          },
     ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    ok = ecrawler_util:unlink_all(),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_info(start_crawl_work,
            #{ materiel_table := MaterielTable
             , contasknum := ConTaskNum
             , max_depth := MaxDepth} = State) when ConTaskNum < ?CONTASKNUM ->
    NewState =
        case start_crawl_work(MaterielTable, MaxDepth) of
            true ->
                erlang:send(erlang:self(), start_crawl_work),
                State#{contasknum := ConTaskNum + 1};
            '$end_of_table' ->
                % erlang:send(erlang:self(), start_crawl_work),
                State;
            _ ->
                erlang:send_after(1000, erlang:self(), start_crawl_work),
                State
        end,
    {noreply, NewState, ?HIBERNATE_TIMEOUT};

handle_info(start_crawl_work, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info({over_one_crawl_execute},
            #{ contasknum := OldConTaskNum
             , max_depth := MaxDepth
             , materiel_table := MaterielTable} = State) ->
    NewState =
        case start_crawl_work(MaterielTable, MaxDepth) of
            true ->
                State;
            '$end_of_table' ->
                State#{contasknum := OldConTaskNum - 1};
            _ ->
                erlang:send(erlang:self(), start_crawl_work),
                State#{contasknum := OldConTaskNum - 1}
        end,
    case {?MNESIA:table_size(MaterielTable), OldConTaskNum} of
        {0, 1} ->
            erlang:send(erlang:self(), {report_finish_work});
        _ ->
            ignore
    end,
    {noreply, NewState, ?HIBERNATE_TIMEOUT};

handle_info({report_finish_work},
            #{taskowner := TaskOwner} = State) ->
    erlang:send(TaskOwner, {finish_work, self()}),
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info({'EXIT', SuspectedMgrPid, _Reson}, State) ->
    case SuspectedMgrPid == maps:get(taskowner, State) of
        true ->
            lager:error(" !! manager pid exit : ~p~n",
                        [erlang:node(SuspectedMgrPid)]),
            ok = ecrawler_util:unlink_all(),
            {stop, normal, State};
        false ->
            {noreply, State, ?HIBERNATE_TIMEOUT}
    end;

handle_info(stop, State) ->
    ok = ecrawler_util:unlink_all(),
    %% dump `terminate(normal, State)` function
    {stop, normal, State};

handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
               [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:error(" ** worker process ~p terminated, reason : ~p",
                [self(), Reason]),
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_crawl_work(MaterielTable, MaxDepth) ->
    start_crawl_work_find_key(?MNESIA:table_all_keys(MaterielTable),
                              MaterielTable, MaxDepth).

start_crawl_work_find_key([], MaterielTable, _) ->
    case ?MNESIA:table_size(MaterielTable) of
        0 ->
            '$end_of_table';
        _ ->
            ignore
    end;
start_crawl_work_find_key([Key | Tail], MaterielTable, MaxDepth) ->
    case check_key_hash(ecrawler_cluster:get_running_nodes(),
                        erlang:node(), Key) of
        true ->
            ?MNESIA:dirty_delete(MaterielTable, Key),
            WorkerPid = erlang:self(),
            F = fun() ->
                    execute_crawl_task(WorkerPid, Key,
                                       MaterielTable, MaxDepth)
                end,
            erlang:spawn_link(F),
            true;
        _ ->
            start_crawl_work_find_key(Tail, MaterielTable, MaxDepth)
    end.

check_key_hash(AllRunningNodes, Node, Key) ->
    NodesLen = erlang:length(AllRunningNodes),
    PHash    = erlang:phash2(Key, NodesLen),
    Node == lists:nth(PHash + 1, lists:sort(AllRunningNodes)).

execute_crawl_task(WorkerPid, Key, MaterielTable, MaxDepth) ->
    {Depth, URL} = Key,
    case Depth + 1 =< MaxDepth of
        true ->
            [begin
                Record = #crawl_materiel{materiel = {Depth + 1, X}},
                ?MNESIA:dirty_write(MaterielTable, Record)
             end || X <- lists:seq(1, URL * 2)],
            ok;
        _ ->
            false
    end,
    timer:sleep(500),
    % io:format("------- ~p~n", [{Depth, URL}]),
    erlang:send(WorkerPid, {over_one_crawl_execute}),
    ok.

%%%===================================================================
%%% eunit test
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
