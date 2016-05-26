-module(c22).

-export([mergesort/1]).
-export([quicksort/1]).
-export([insertsort/1]).

mergesort([] = L) -> L;
mergesort([_] = L) -> L;
mergesort(L) when erlang:length(L) =< 15 ->
	c2:insertsort(L);
mergesort(L) ->
	{L1, L2} = lists:split(erlang:round(erlang:length(L)/2), L),
	merge(mergesort(L1), mergesort(L2)).

merge([], L2) -> L2;
merge(L1, []) -> L1;
merge([H1 | T1], [H2 | T2]) when H1 < H2 ->
	[H1 | merge(T1, [H2 | T2])];
merge([H1 | T1], [H2 | T2]) ->
	[H2 | merge([H1 | T1], T2)].


quicksort([] = L) -> L;
quicksort([_] = L) -> L;
quicksort([H | T]) ->
	{S, B} = quicksort(H, T, [], []),
	S ++ [H] ++ B.

quicksort(_H, [], Res1, Res2) ->
	{Res1, Res2};
quicksort(H, [Head | Tail], Res1, Res2) when Head > H ->
	quicksort(H, Tail, Res1, [Head | Res2]);
quicksort(H, [Head | Tail], Res1, Res2) ->
	quicksort(H, Tail, [Head | Res1], Res2).

insertsort([] = L) -> L;
insertsort([_] = L) -> L;
insertsort(L) ->
    insertsort(L, []).

insertsort([], Res) ->
    Res;
insertsort([H | T], Res) ->
	NewRes =
		case binary_search(H, Res) of
			[] ->
				[H | Res];
			{nil, _} ->
				[H | Res];
			{_, nil} ->
				Res ++ [H];
			{Index, _} ->
				{Res1, Res2} = lists:split(Index, Res),
				Res1 ++ [H] ++ Res2
		end,
	insertsort(T, NewRes).

binary_search(_Element, []) ->
	[];
binary_search(Element, List) ->
	[Head | _] = List,
	[Tail | _] = lists:reverse(List),
	case {Element =< Head, Element >= Tail} of
		{true, _} ->
			{nil, 1};
		{_, true} ->
			{erlang:length(List), nil};
		_ ->
			MidIndex = erlang:round(erlang:length(List)/2),
			MidEleme = lists:nth(MidIndex, List),
			binary_search_do(Element, List, MidIndex, MidEleme)
	end.

binary_search_do(Element, List, MidIndex, MidEleme) when Element =< MidEleme ->
	PrevEleme = lists:nth(MidIndex - 1, List),
	case Element >= PrevEleme of
		true ->
			{MidIndex - 1, MidIndex};
		false ->
			NewMidIndex = erlang:round((1+MidIndex)/2),
			NewMidEleme = lists:nth(NewMidIndex, List),
			binary_search_do(Element, List, NewMidIndex, NewMidEleme)
	end;
binary_search_do(Element, List, MidIndex, _MidEleme) ->
	NewMidIndex = erlang:round((MidIndex + erlang:length(List))/2),
	NewMidEleme = lists:nth(NewMidIndex, List),
	binary_search_do(Element, List, NewMidIndex, NewMidEleme).


