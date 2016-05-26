-module(c23).

-export([mergesort/1]).

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
