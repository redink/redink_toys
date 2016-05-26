-module(c2).

-export([exch/3, issorted/1, nth/2]).
-export([selectsort/1]).
-export([insertsort/1]).
-export([bubblesort/1]).

-export([smallest/1]).


bubblesort([] = L) -> L;
bubblesort([_] = L) -> L;
bubblesort(L) ->
    bubblesort(L, []).

bubblesort(L, Res) ->
    bubblesort(L, Res, []).

bubblesort([], Res, []) ->
    Res;
bubblesort([X], Res, Help) ->
    bubblesort(Help, [X | Res], []);
bubblesort([X, Y | T], Res, Help) when X > Y ->
    bubblesort([X | T], Res, Help ++ [Y]);
bubblesort([X | T], Res, Help) ->
    bubblesort(T, Res, Help ++ [X]).

insertsort([] = L) -> L;
insertsort([_] = L) -> L;
insertsort(L) ->
    insertsort(L, []).

insertsort([], Res) ->
    Res;
insertsort([H | T], Res) ->
    insertsort(T, insertsort_do(H, Res, [])).

insertsort_do(Insert0, [], Help) ->
    Help ++ [Insert0];
insertsort_do(Insert0, [H | _] = L, Help) when Insert0 < H ->
    Help ++ [Insert0] ++ L;
insertsort_do(Insert0, [H | T], Help) ->
    insertsort_do(Insert0, T, Help ++ [H]).

selectsort([] = L) -> L;
selectsort([_] = L) -> L;
selectsort(L) -> selectsort(L, []).

selectsort([], Res) ->
    Res;
selectsort(L, Res) ->
    [H | T] = exch(L, 1, smallest(L)),
    selectsort(T, Res ++ [H]).

smallest([Min | T]) ->
    smallest(Min, T, 1).
smallest(_Min, [], Index) ->
    Index;
smallest(Min, [H | T], Index) when Min > H ->
    smallest(H, T, Index + 1);
smallest(Min, [_H | T], Index) ->
    smallest(Min, T, Index).

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


issorted([]) ->
    true;
issorted([_]) ->
    true;
issorted([X1, X2 | T]) when X1 =< X2 ->
    issorted([X2 | T]);
issorted(_) ->
    false.

nth(N, L) ->
    nth(L, N, 1).

nth([], _, _) ->
    erlang:throw(error);
nth([H | _], N, Index) when N == Index ->
    H;
nth([_ | T], N, Index) ->
    nth(T, N, Index + 1).
