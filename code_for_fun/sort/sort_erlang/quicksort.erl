-module(quicksort).

-export([sort/1]).

sort([]) ->
	[];
sort([X]) ->
	[X];
sort([H | T]) ->
	{S, B} = split(H, T),
	sort(S) ++ [H] ++ sort(B).

split(H, L) ->
	split(L, H, [], []).

split([], _, S, B) ->
	{S, B};
split([H | T], P, S, B) when H > P ->
	split(T, P, S, [H | B]);
split([H | T], P, S, B) ->
	split(T, P, [H | S], B).