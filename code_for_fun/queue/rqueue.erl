-module(rqueue).

-compile(export_all).

new() ->
	{[], []}.

in(X, {In, Out}) ->
	{[X | In], Out}.

out({[], []} = Q) ->
	{empty, Q};
out({[], [X | Out]}) ->
	{{value, X}, {[], Out}};
out({[X], Out}) ->
	{{value, X}, {[], Out}};
out({In, Out}) ->
	{X, NewOut} = out_do(In, Out),
	{{value, X}, {[], NewOut}}.

out_do([X], NewOut) ->
	{X, NewOut};
out_do([H | T], NewOut) ->
	out_do(T, [H | NewOut]).

