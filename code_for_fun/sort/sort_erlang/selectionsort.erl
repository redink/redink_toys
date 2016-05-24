-module(selectionsort).

-export([sort/1]).

sort([] = L) ->
	L;
sort([_] = L) ->
	L;
sort(L) ->
	sort(L, []).

sort([], Res) ->
	lists:reverse(Res);
sort([Head0 | Tail0], Res) ->
	{Min, Tail} = sort_do(Head0, Tail0, []),
	sort(Tail, [Min | Res]).

sort_do(Min, [], Res) ->
	{Min, lists:reverse(Res)};
sort_do(Min, [H | T], Res) when Min > H ->
	sort_do(H, T, [Min | Res]);
sort_do(Min, [H | T], Res) ->
	sort_do(Min, T, [H | Res]).

% 10> selectionsort:sort([4,3,7,1,2,9]).
% <0.35.0> selectionsort:sort([4, 3, 7, 1, 2, 9])
% <0.35.0> | selectionsort:sort([4, 3, 7, 1, 2, 9], [])
% <0.35.0> | | selectionsort:sort_do(4, [3, 7, 1, 2, 9], [])
% <0.35.0> | | | selectionsort:sort_do(3, [7, 1, 2, 9], [4])
% <0.35.0> | | | | selectionsort:sort_do(3, [1, 2, 9], [7, 4])
% <0.35.0> | | | | | selectionsort:sort_do(1, [2, 9], [3, 7, 4])
% <0.35.0> | | | | | | selectionsort:sort_do(1, [9], [2, 3, 7, 4])
% <0.35.0> | | | | | | | selectionsort:sort_do(1, [], [9, 2, 3, 7, 4])
% <0.35.0> | | | | | | | {1,[4,7,3,2,9]}
% <0.35.0> | | | | | | {1,[4,7,3,2,9]}
% <0.35.0> | | | | | {1,[4,7,3,2,9]}
% <0.35.0> | | | | {1,[4,7,3,2,9]}
% <0.35.0> | | | {1,[4,7,3,2,9]}
% <0.35.0> | | {1,[4,7,3,2,9]}
% <0.35.0> | | selectionsort:sort([4, 7, 3, 2, 9], [1])
% <0.35.0> | | | selectionsort:sort_do(4, [7, 3, 2, 9], [])
% <0.35.0> | | | | selectionsort:sort_do(4, [3, 2, 9], [7])
% <0.35.0> | | | | | selectionsort:sort_do(3, [2, 9], [4, 7])
% <0.35.0> | | | | | | selectionsort:sort_do(2, [9], [3, 4, 7])
% <0.35.0> | | | | | | | selectionsort:sort_do(2, [], [9, 3, 4, 7])
% <0.35.0> | | | | | | | {2,[7,4,3,9]}
% <0.35.0> | | | | | | {2,[7,4,3,9]}
% <0.35.0> | | | | | {2,[7,4,3,9]}
% <0.35.0> | | | | {2,[7,4,3,9]}
% <0.35.0> | | | {2,[7,4,3,9]}
% <0.35.0> | | | selectionsort:sort([7, 4, 3, 9], [2, 1])
% <0.35.0> | | | | selectionsort:sort_do(7, [4, 3, 9], [])
% <0.35.0> | | | | | selectionsort:sort_do(4, [3, 9], [7])
% <0.35.0> | | | | | | selectionsort:sort_do(3, [9], [4, 7])
% <0.35.0> | | | | | | | selectionsort:sort_do(3, [], [9, 4, 7])
% <0.35.0> | | | | | | | {3,[7,4,9]}
% <0.35.0> | | | | | | {3,[7,4,9]}
% <0.35.0> | | | | | {3,[7,4,9]}
% <0.35.0> | | | | {3,[7,4,9]}
% <0.35.0> | | | | selectionsort:sort([7, 4, 9], [3, 2, 1])
% <0.35.0> | | | | | selectionsort:sort_do(7, [4, 9], [])
% <0.35.0> | | | | | | selectionsort:sort_do(4, [9], [7])
% <0.35.0> | | | | | | | selectionsort:sort_do(4, [], [9, 7])
% <0.35.0> | | | | | | | {4,[7,9]}
% <0.35.0> | | | | | | {4,[7,9]}
% <0.35.0> | | | | | {4,[7,9]}
% <0.35.0> | | | | | selectionsort:sort([7, 9], [4, 3, 2, 1])
% <0.35.0> | | | | | | selectionsort:sort_do(7, [9], [])
% <0.35.0> | | | | | | | selectionsort:sort_do(7, [], [9])
% <0.35.0> | | | | | | | {7,"\t"}
% <0.35.0> | | | | | | {7,"\t"}
% <0.35.0> | | | | | | selectionsort:sort([9], [7, 4, 3, 2, 1])
% <0.35.0> | | | | | | | selectionsort:sort_do(9, [], [])
% <0.35.0> | | | | | | | {9,[]}
% <0.35.0> | | | | | | | selectionsort:sort([], [9, 7, 4, 3, 2, 1])
% <0.35.0> | | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | | [1,2,3,4,7,9]
% <0.35.0> | | | | [1,2,3,4,7,9]
% <0.35.0> | | | [1,2,3,4,7,9]
% <0.35.0> | | [1,2,3,4,7,9]
% <0.35.0> | [1,2,3,4,7,9]
% <0.35.0> [1,2,3,4,7,9]
