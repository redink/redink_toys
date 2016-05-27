-module(c23).

-export([mergesort/1]).
-export([start/0]).
-export([swim/1, sink/1]).
-export([heapsort/1]).

mergesort(L) when erlang:length(L) =< 1 -> L;
mergesort(L) when erlang:length(L) =< 15 ->
    c2:insertsort(L);
mergesort(L) ->
    {L1, L2} = lists:split(erlang:round(erlang:length(L)/2), L),
    merge(mergesort(L1), mergesort(L2), []).

merge([], L2, Res) -> lists:reverse(Res) ++ L2;
merge(L1, [], Res) -> lists:reverse(Res) ++ L1;
merge([H1 | T1], [H2 | T2], Res) when H1 < H2 ->
    merge(T1, [H2 | T2], [H1 | Res]);
merge([H1 | T1], [H2 | T2], Res) ->
    merge([H1 | T1], T2, [H2 | Res]).


heapsort(L) when erlang:length(L) =< 1 -> L;
heapsort(L) ->
    heapsort_2(heapsort_1(L, []), []).

heapsort_1([], Res) ->
    Res;
heapsort_1([H | T], Res) ->
    heapsort_1(T, sink([H | Res])).

heapsort_2([], Res) ->
    Res;
heapsort_2([H | T], Res) ->
    heapsort_2(heapsort_1(T, []), [H | Res]).

start() ->
    List1 = swim([0]),
    List1 = sink([0]),

    [H2 | _] = List2 = swim(List1 ++ [1]),
    [H2 | _] = sink([1 | List1]),

    [H3 | _] = List3 = swim(List2 ++ [2]),
    [H3 | _] = sink([2 | List2]),

    [H4 | _] = List4 = swim(List3 ++ [3]),
    [H4 | _] = sink([3 | List3]),

    [H5 | _] = List5 = swim(List4 ++ [4]),
    [H5 | _] = sink([4 | List4]),

    List5.

swim(List) ->
    LLen = erlang:length(List),
    swim(List, LLen).

swim(List, K) when K > 1 ->
    case nth(K, List) > nth(K div 2, List) of
        true ->
            swim(exch(List, K div 2, K), K div 2);
        _ ->
            List
    end;
swim(List, _K) ->
    List.

sink(List) ->
    LLen = erlang:length(List),
    sink(List, 1, LLen).

sink(List, K, LLen) when 2* K =< LLen ->
    case nth(K, List) < nth(2 * K, List) of
        true ->
            sink(exch(List, K, 2 * K), K * 2, LLen);
        _ ->
            List
    end;
sink(List, _, _) ->
    List.

exch(L, I, J) when I == J ->
    L;
exch(L, I, J) when I > J ->
    exch(L, J, I);
exch(L, I, J) when I < J ->
    case erlang:length(L) >= J of
        true ->
            exch(L, I, J, 1, [], [], [], 0, 0);
        _ ->
            error
    end.

exch([], _, _, _, Res1, Res2, Res3, II, JJ) ->
    lists:reverse(Res3 ++ [II] ++ Res2 ++ [JJ] ++ Res1);
exch([H | T], I, J, Index, Res1, Res2, Res3, II, JJ) ->
    if
        I > Index ->
            exch(T, I, J, Index + 1, [H | Res1], Res2, Res3, II, JJ);
        I == Index ->
            exch(T, I, J, Index + 1, Res1, Res2, Res3, H, JJ);
        I < Index andalso J > Index ->
            exch(T, I, J, Index + 1, Res1, [H | Res2], Res3, II, JJ);
        J == Index ->
            exch(T, I, J, Index + 1, Res1, Res2, Res3, II, H);
        J < Index ->
            exch(T, I, J, Index + 1, Res1, Res2, [H | Res3], II, JJ)
    end.

nth(N, L) ->
    nth(L, N, 1).

nth([], _, _) ->
    erlang:throw(error);
nth([H | _], N, Index) when N == Index ->
    H;
nth([_ | T], N, Index) ->
    nth(T, N, Index + 1).
