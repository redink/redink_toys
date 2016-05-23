-module(selectionsort).

-export([sort/1]).

sort(L) ->
	sort(L, []).
sort([], Res) ->
	lists:reverse(Res);
sort([Head0 | Tail0], Res) ->
	{Head, Tail} = process_head_tail(Head0, Tail0),
	sort(Tail, [Head | Res]).

process_head_tail(Head, Tail) ->
	process_head_tail(Tail, Head, []).

process_head_tail([], Min, NewTail) ->
	{Min, lists:reverse(NewTail)};
process_head_tail([Head | Tail], Min, NewTail) when Head < Min ->
	process_head_tail(Tail, Head, [Min | NewTail]);
process_head_tail([Head | Tail], Min, NewTail) ->
	process_head_tail(Tail, Min, [Head | NewTail]).
