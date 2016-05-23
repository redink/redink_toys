-module(bubblesort).
-export([sort/1, sort/1]).

sort(L) ->
	sort(L, [], []).

sort([], [], Res) ->
	Res;
sort([], Temp, Res) ->
	[Sorted | Other] = Temp,
	sort(lists:reverse(Other), [], [Sorted | Res]);
sort([X], Temp, Res) ->
	sort(lists:reverse(Temp), [], [X | Res]);
sort([X, Y | T], Temp, Res) when X > Y ->
	sort([X | T], [Y | Temp], Res);
sort([X | T], Temp, Res) ->
	sort(T, [X | Temp], Res).
