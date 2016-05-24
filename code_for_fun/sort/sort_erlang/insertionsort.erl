-module(insertionsort).

-export([sort/1]).

sort([] = L) ->
	L;
sort([_] = L) ->
	L;
sort(L) ->
	sort(L, []).

sort([], InsertedList) ->
	InsertedList;
sort([H | T], InsertedList) ->
	sort(T, sort_do(H, InsertedList, [])).

sort_do(Insert0, [], Res) ->
	Res ++ [Insert0];
sort_do(Insert0, [H | _] = InsertedList, Res) when Insert0 < H ->
	Res ++ [Insert0] ++ InsertedList;
sort_do(Insert0, [H | T], Res) ->
	sort_do(Insert0, T, Res ++ [H]).

% 4> insertionsort:sort([4,3,7,1,2,9]).
% <0.35.0> insertionsort:sort([4, 3, 7, 1, 2, 9])
% <0.35.0> | insertionsort:sort([4, 3, 7, 1, 2, 9], [])
% <0.35.0> | | insertionsort:sort_do(4, [], [])
% <0.35.0> | | [4]
% <0.35.0> | | insertionsort:sort([3, 7, 1, 2, 9], [4])
% <0.35.0> | | | insertionsort:sort_do(3, [4], [])
% <0.35.0> | | | [3,4]
% <0.35.0> | | | insertionsort:sort([7, 1, 2, 9], [3, 4])
% <0.35.0> | | | | insertionsort:sort_do(7, [3, 4], [])
% <0.35.0> | | | | | insertionsort:sort_do(7, [4], [3])
% <0.35.0> | | | | | | insertionsort:sort_do(7, [], [3, 4])
% <0.35.0> | | | | | | [3,4,7]
% <0.35.0> | | | | | [3,4,7]
% <0.35.0> | | | | [3,4,7]
% <0.35.0> | | | | insertionsort:sort([1, 2, 9], [3, 4, 7])
% <0.35.0> | | | | | insertionsort:sort_do(1, [3, 4, 7], [])
% <0.35.0> | | | | | [1,3,4,7]
% <0.35.0> | | | | | insertionsort:sort([2, 9], [1, 3, 4, 7])
% <0.35.0> | | | | | | insertionsort:sort_do(2, [1, 3, 4, 7], [])
% <0.35.0> | | | | | | | insertionsort:sort_do(2, [3, 4, 7], [1])
% <0.35.0> | | | | | | | [1,2,3,4,7]
% <0.35.0> | | | | | | [1,2,3,4,7]
% <0.35.0> | | | | | | insertionsort:sort([9], [1, 2, 3, 4, 7])
% <0.35.0> | | | | | | | insertionsort:sort_do(9, [1, 2, 3, 4, 7], [])
% <0.35.0> | | | | | | | | insertionsort:sort_do(9, [2, 3, 4, 7], [1])
% <0.35.0> | | | | | | | | | insertionsort:sort_do(9, [3, 4, 7], [1, 2])
% <0.35.0> | | | | | | | | | | insertionsort:sort_do(9, [4, 7], [1, 2, 3])
% <0.35.0> | | | | | | | | | | | insertionsort:sort_do(9, [7], [1, 2, 3, 4])
% <0.35.0> | | | | | | | | | | | | insertionsort:sort_do(9, [], [1, 2, 3, 4, 7])
% <0.35.0> | | | | | | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | | insertionsort:sort([], [1, 2, 3, 4, 7, 9])
% <0.35.0> | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | [1,2,3,4,7,9]
% <0.35.0> | | | [1,2,3,4,7,9]
% <0.35.0> | | [1,2,3,4,7,9]
% <0.35.0> | [1,2,3,4,7,9]
% <0.35.0> [1,2,3,4,7,9]
