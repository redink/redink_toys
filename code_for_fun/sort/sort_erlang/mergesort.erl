-module(mergesort).

-export([sort/1]).

sort([] = L) ->
	L;
sort([_] = L) ->
	L;
sort(L) ->
	{F, S} = lists:split(erlang:round(erlang:length(L)/2), L),
	merge(sort(F), sort(S)).

merge([], L) ->
	L;
merge(L, []) ->
	L;
merge([H1 | T1], [H2 | _] = L2) when H1 < H2 ->
	[H1 | merge(T1, L2)];
merge(L1, [H2 | T2]) ->
	[H2 | merge(L1, T2)].