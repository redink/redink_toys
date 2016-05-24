-module(quicksort).

-export([sort/1]).

sort([] = L) ->
	L;
sort([_] = L) ->
	L;
sort([Head | T]) ->
	{S, B} = sort_do(Head, T, [], []),
	sort(S) ++ [Head] ++ sort(B).

sort_do(_, [], S, B) ->
	{S, B};
sort_do(Head, [H | T], S, B) when H < Head ->
	sort_do(Head, T, [H | S], B);
sort_do(Head, [H | T], S, B) ->
	sort_do(Head, T, S, [H | B]).

% 8> quicksort:sort([4,3,7,1,2,9]).
% <0.35.0> quicksort:sort([4, 3, 7, 1, 2, 9])
% <0.35.0> | quicksort:sort_do(4, [3, 7, 1, 2, 9], [], [])
% <0.35.0> | | quicksort:sort_do(4, [7, 1, 2, 9], [3], [])
% <0.35.0> | | | quicksort:sort_do(4, [1, 2, 9], [3], [7])
% <0.35.0> | | | | quicksort:sort_do(4, [2, 9], [1, 3], [7])
% <0.35.0> | | | | | quicksort:sort_do(4, [9], [2, 1, 3], [7])
% <0.35.0> | | | | | | quicksort:sort_do(4, [], [2, 1, 3], [9, 7])
% <0.35.0> | | | | | | {[2,1,3],[9,7]}
% <0.35.0> | | | | | {[2,1,3],[9,7]}
% <0.35.0> | | | | {[2,1,3],[9,7]}
% <0.35.0> | | | {[2,1,3],[9,7]}
% <0.35.0> | | {[2,1,3],[9,7]}
% <0.35.0> | {[2,1,3],[9,7]}
% <0.35.0> | quicksort:sort([2, 1, 3])
% <0.35.0> | | quicksort:sort_do(2, [1, 3], [], [])
% <0.35.0> | | | quicksort:sort_do(2, [3], [1], [])
% <0.35.0> | | | | quicksort:sort_do(2, [], [1], [3])
% <0.35.0> | | | | {[1],[3]}
% <0.35.0> | | | {[1],[3]}
% <0.35.0> | | {[1],[3]}
% <0.35.0> | | quicksort:sort([1])
% <0.35.0> | | [1]
% <0.35.0> | | quicksort:sort([3])
% <0.35.0> | | [3]
% <0.35.0> | [1,2,3]
% <0.35.0> | quicksort:sort([9, 7])
% <0.35.0> | | quicksort:sort_do(9, [7], [], [])
% <0.35.0> | | | quicksort:sort_do(9, [], [7], [])
% <0.35.0> | | | {[7],[]}
% <0.35.0> | | {[7],[]}
% <0.35.0> | | quicksort:sort([7])
% <0.35.0> | | [7]
% <0.35.0> | | quicksort:sort([])
% <0.35.0> | | []
% <0.35.0> | [7,9]
% <0.35.0> [1,2,3,4,7,9]