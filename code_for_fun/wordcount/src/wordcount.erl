-module(wordcount).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start/2]).
-export([worker_execute/3]).
-export([insert_main_ets_table/2]).

-define(BUFSIZE, 1048576).
-define(FILEMODE, [binary, read, {read_ahead, ?BUFSIZE}]).
-define(SPLIT_CHAR_LIST, [<<"\n">>, <<" ">>, <<"\"">>,
                          <<"!">>, <<"&">>, <<".">>,
                          <<",">>, <<":">>, <<"--">>]).

start(_, []) ->
    "NodeList can not is empty";
start(FileName, NodeList) ->
    %% open the file
    {ok, FD}      = file:open(FileName, ?FILEMODE),
    MainEts       = 
        ets:new(mainets, [public,
                          {write_concurrency, true}]),
    TaskOwnerNode = erlang:node(),
    TaskResList   =
        spawn_worker_process(file:read_line(FD), FD, NodeList,
                             TaskOwnerNode, MainEts, []),
    [begin
        {WorkerNode, WorkerPid} = task:await(TaskRef),
        io:format(" ** One Task return it's result from Node : ~p, worker pid : ~p ~n",
                  [WorkerNode, WorkerPid]),
        ok
    end || TaskRef <- TaskResList],
    Ms = ets:fun2ms(fun({_, Times} = WordCount) when Times > 10 -> WordCount end),
    FinalResult = ets:select(MainEts, Ms),
    true = ets:delete(MainEts),
    FinalResult.

spawn_worker_process(eof, _FD, _NodeList, _TaskOwnerNode, _MainEts, Res) ->
    Res;
spawn_worker_process({ok, Data}, FD, NodeList, TaskOwnerNode, MainEts, Res) ->
    Node    = get_random_node(NodeList),
    TaskRef = task:async(Node, ?MODULE, worker_execute,
                         [Data, TaskOwnerNode, MainEts]),
    spawn_worker_process(file:read_line(FD), FD, NodeList,
                         TaskOwnerNode, MainEts, [TaskRef | Res]).

get_random_node(NodeList) ->
    NodeListLen = erlang:length(NodeList),
    lists:nth(get_random(NodeListLen), NodeList).

get_random(Num) ->
    {Res, _} = random:uniform_s(Num, erlang:now()),
    Res.

worker_execute(Data, TaskOwnerNode, MainEts) ->
    %% <<"this is an example\n">>
    %% binary split
    [_ | WordList] =
        lists:reverse(binary:split(Data, ?SPLIT_CHAR_LIST, [global])),
    TempEts = ets:new(tempets, [set]),
    ok = lists:foreach(fun(UK) ->
                        case K = string:to_lower(erlang:binary_to_list(UK)) of
                            [] ->
                                ingore;
                            _ ->
                                true = etsCountAdd(TempEts, K, {2, 1}, {K, 1})
                        end
                       end, WordList),
    rpc:call(TaskOwnerNode, ?MODULE,
             insert_main_ets_table, [MainEts, ets:tab2list(TempEts)]),
    ets:delete(TempEts),
    {erlang:node(), erlang:self()}.

insert_main_ets_table(MainEts, WorkerResult) ->
    [etsCountAdd(MainEts, K, {2, V}, {K, V}) || {K, V} <- WorkerResult],
    ok.

etsCountAdd(EtsTab, Key, UpdateValue, InsertValue) ->
    try ets:insert_new(EtsTab, InsertValue) of
        false ->
            ets:update_counter(EtsTab, Key, UpdateValue),
            true;
        _ ->
            true
    catch _:_ ->
        false
    end.
