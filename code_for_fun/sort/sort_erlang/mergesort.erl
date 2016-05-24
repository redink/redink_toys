-module(mergesort).

-export([sort/1]).

sort([] = L) ->
	L;
sort([_] = L) ->
	L;
sort(L) ->
	{L1, L2} = lists:split(erlang:round(erlang:length(L)/2), L),
	merge(sort(L1), sort(L2)).


merge([], L2) ->
	L2;
merge(L1, []) ->
	L1;
merge([H1 | T1], [H2 | T2]) when H1 < H2 ->
	[H1 | merge(T1, [H2 | T2])];
merge([H1 | T1], [H2 | T2]) ->
	[H2 | merge([H1 | T1], T2)].

% 6> mergesort:sort([4,3,7,1,2,9]).
% <0.35.0> mergesort:sort([4, 3, 7, 1, 2, 9])
% <0.35.0> | mergesort:sort([4, 3, 7])
% <0.35.0> | | mergesort:sort([4, 3])
% <0.35.0> | | | mergesort:sort([4])
% <0.35.0> | | | [4]
% <0.35.0> | | | mergesort:sort([3])
% <0.35.0> | | | [3]
% <0.35.0> | | | mergesort:merge([4], [3])
% <0.35.0> | | | | mergesort:merge([4], [])
% <0.35.0> | | | | [4]
% <0.35.0> | | | [3,4]
% <0.35.0> | | [3,4]
% <0.35.0> | | mergesort:sort([7])
% <0.35.0> | | [7]
% <0.35.0> | | mergesort:merge([3, 4], [7])
% <0.35.0> | | | mergesort:merge([4], [7])
% <0.35.0> | | | | mergesort:merge([], [7])
% <0.35.0> | | | | [7]
% <0.35.0> | | | [4,7]
% <0.35.0> | | [3,4,7]
% <0.35.0> | [3,4,7]
% <0.35.0> | mergesort:sort([1, 2, 9])
% <0.35.0> | | mergesort:sort([1, 2])
% <0.35.0> | | | mergesort:sort([1])
% <0.35.0> | | | [1]
% <0.35.0> | | | mergesort:sort([2])
% <0.35.0> | | | [2]
% <0.35.0> | | | mergesort:merge([1], [2])
% <0.35.0> | | | | mergesort:merge([], [2])
% <0.35.0> | | | | [2]
% <0.35.0> | | | [1,2]
% <0.35.0> | | [1,2]
% <0.35.0> | | mergesort:sort([9])
% <0.35.0> | | "\t"
% <0.35.0> | | mergesort:merge([1, 2], [9])
% <0.35.0> | | | mergesort:merge([2], [9])
% <0.35.0> | | | | mergesort:merge([], [9])
% <0.35.0> | | | | "\t"
% <0.35.0> | | | [2,9]
% <0.35.0> | | [1,2,9]
% <0.35.0> | [1,2,9]
% <0.35.0> | mergesort:merge([3, 4, 7], [1, 2, 9])
% <0.35.0> | | mergesort:merge([3, 4, 7], [2, 9])
% <0.35.0> | | | mergesort:merge([3, 4, 7], [9])
% <0.35.0> | | | | mergesort:merge([4, 7], [9])
% <0.35.0> | | | | | mergesort:merge([7], [9])
% <0.35.0> | | | | | | mergesort:merge([], [9])
% <0.35.0> | | | | | | "\t"
% <0.35.0> | | | | | [7,9]
% <0.35.0> | | | | [4,7,9]
% <0.35.0> | | | [3,4,7,9]
% <0.35.0> | | [2,3,4,7,9]
% <0.35.0> | [1,2,3,4,7,9]
% <0.35.0> [1,2,3,4,7,9]
