-module(merge_sort).

-export([merge_sort/1, merge_sort2/1]).
-export([merge_key_sort/2, merge_key_sort2/2]).

%% @doc merge sort
-spec merge_sort(list()) -> list().
merge_sort([]) ->
    [];
merge_sort([ X | [] ]) ->
    [X];
merge_sort(List) ->
    {FList, SList} = lists:split(erlang:round(erlang:length(List)/2), List),
    lists_merge(merge_sort(FList), merge_sort(SList)).

%% @doc merge sort
-spec merge_sort2(list()) -> list().
merge_sort2([]) ->
    [];
merge_sort2([ X | [] ]) ->
    [X];
merge_sort2(List) ->
    {FList, SList} = lists:split(erlang:round(erlang:length(List)/2), List),
    lists_merge2(merge_sort2(FList), merge_sort2(SList)).

%% @doc merge sort by index
-spec merge_key_sort(list(), non_neg_integer()) -> list().
merge_key_sort([], _Index) ->
    [];
merge_key_sort([ X | [] ], _Index) ->
    [X];
merge_key_sort(List, Index) ->
    {FList, SList} = lists:split(erlang:round(erlang:length(List)/2), List),
    lists_key_merge(merge_key_sort(FList, Index),
                    merge_key_sort(SList, Index),
                    Index).

%% @doc merge sort by index
-spec merge_key_sort2(list(), non_neg_integer()) -> list().
merge_key_sort2([], _Index) ->
    [];
merge_key_sort2([ X | [] ], _Index) ->
    [X];
merge_key_sort2(List, Index) ->
    {FList, SList} = lists:split(erlang:round(erlang:length(List)/2), List),
    lists_key_merge2(merge_key_sort2(FList, Index),
                     merge_key_sort2(SList, Index),
                     Index).

%% @private
%% merge [1,2,10] [3,4,5]
%% [1] ++ merge [2,10] [3,4,5]
%% [1,2] ++ merge [10] [3,4,5]
%% [1,2,3] ++ merge [10] [4,5]
%% [1,2,3,4] ++ merge [10] [5]
%% [1,2,3,4,5] ++ merge [10] []
%% [1,2,3,4,5] ++ [10]
%% -> [1,2,3,4,5,10]
lists_merge([] = _L1, L2) ->
    L2;
lists_merge(L1, [] = _L2) ->
    L1;
lists_merge([H1 | T1], [H2 | _] = L2) when H1 =< H2 ->
    [H1 | lists_merge(T1, L2)];
lists_merge(L1, [H2 | T2]) ->
    [H2 | lists_merge(L1, T2)].

lists_key_merge([] = _L1, L2, _Index) ->
    L2;
lists_key_merge(L1, [] = _L2, _Index) ->
    L1;
lists_key_merge([H1 | T1], [H2 | _] = L2, Index)
        when erlang:element(Index, H1) =< erlang:element(Index, H2) ->
    [H1 | lists_key_merge(T1, L2, Index)];
lists_key_merge([H1 | _] = L1, [H2 | T2], Index)
        when erlang:element(Index, H1) > erlang:element(Index, H2) ->
    [H2 | lists_key_merge(L1, T2, Index)].

%% @private
%% merge [1,2,10] [3,4,5]
%% merge [10,2,1] [5,4,3] []
%% merge [2,1] [5,4,3] [10]
%% merge [2,1] [4,3] [5,10]
%% merge [2,1] [3] [4,5,10]
%% merge [2,1] [] [3,4,5,10]
%% [1,2] ++ [3,4,5,10]
%% -> [1,2,3,4,5,10]
lists_merge2(L1, L2) ->
    lists_merge_do(lists:reverse(L1), lists:reverse(L2), []).
lists_merge_do([], L2, Tmp) ->
    lists:reverse(L2) ++ Tmp;
lists_merge_do(L1, [], Tmp) ->
    lists:reverse(L1) ++ Tmp;
lists_merge_do([H1 | T1], [H2 | _] = L2, Tmp) when H1 >= H2 ->
    lists_merge_do(T1, L2, [H1 | Tmp]);
lists_merge_do(L1, [H2 | T2], Tmp) ->
    lists_merge_do(L1, T2, [H2 | Tmp]).

lists_key_merge2(L1, L2, Index) ->
    lists_key_merge2(lists:reverse(L1), lists:reverse(L2), Index, []).
lists_key_merge2([], L2, _Index, Tmp) ->
    lists:reverse(L2) ++ Tmp;
lists_key_merge2(L1, [], _Index, Tmp) ->
    lists:reverse(L1) ++ Tmp;
lists_key_merge2([H1 | T1], [H2 | _] = L2, Index, Tmp)
        when erlang:element(Index, H1) >= erlang:element(Index, H2) ->
    lists_key_merge2(T1, L2, Index, [H1 | Tmp]);
lists_key_merge2([H1 | _] = L1, [H2 | T2], Index, Tmp)
        when erlang:element(Index, H1) < erlang:element(Index, H2) ->
    lists_key_merge2(L1, T2, Index, [H2 | Tmp]).
