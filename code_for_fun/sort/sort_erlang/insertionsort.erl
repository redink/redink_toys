-module(insertionsort).

-export([sort/1]).

sort([]) ->
	[];
sort([_] = L) ->
	L;
sort(L) ->
	sort(L, []).

sort([], Res) ->
	Res;
sort([H | T], Res) ->
	sort(T, sort_do(H, Res)).

sort_do(H, []) ->
	[H];
sort_do(H, Res) ->
	sort_do(Res, H, []).
	
sort_do([], Inser0, Res) ->
	lists:reverse(Res) ++ [Inser0];
sort_do([H | T], Inser0, Res) when Inser0 < H ->
	lists:reverse(Res) ++ [Inser0, H | T];
sort_do([H | T], Inser0, Res) ->
	sort_do(T, Inser0, [H | Res]).
