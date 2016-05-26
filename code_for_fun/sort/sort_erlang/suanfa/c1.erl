-module(c1).

-export([gcd/2, binary_search/2]).
-export(['fun_1.1.3'/3]).
-export([eval/1, threesum/1]).

gcd(P, Q) when P >= 0, Q >= 0, P >= Q ->
	gcd_do(P, Q);
gcd(P, Q) when P >= 0, Q >= 0 ->
	gcd_do(Q, P).

gcd_do(P, 0) ->
	P;
gcd_do(0, Q) ->
	Q;
gcd_do(P, Q) ->
	gcd(P rem Q, Q).


binary_search(_Element, []) ->
	[];
binary_search(Element, List) ->
	[Head | _] = List,
	[Tail | _] = lists:reverse(List),
	case {Element =< Head, Element >= Tail} of
		{true, _} ->
			{nil, Head};
		{_, true} ->
			{Tail, nil};
		_ ->
			MidIndex = erlang:round(erlang:length(List)/2),
			MidEleme = lists:nth(MidIndex, List),
			binary_search_do(Element, List, MidIndex, MidEleme)
	end.

binary_search_do(Element, List, MidIndex, MidEleme) when Element =< MidEleme ->
	PrevEleme = lists:nth(MidIndex - 1, List),
	case Element >= PrevEleme of
		true ->
			{PrevEleme, MidEleme};
		false ->
			NewMidIndex = erlang:round((1+MidIndex)/2),
			NewMidEleme = lists:nth(NewMidIndex, List),
			binary_search_do(Element, List, NewMidIndex, NewMidEleme)
	end;
binary_search_do(Element, List, MidIndex, _MidEleme) ->
	NewMidIndex = erlang:round((MidIndex + erlang:length(List))/2),
	NewMidEleme = lists:nth(NewMidIndex, List),
	binary_search_do(Element, List, NewMidIndex, NewMidEleme).


'fun_1.1.3'(A, A, A) ->
	equal;
'fun_1.1.3'(_, _, _) ->
	'not equal'.

eval(L) ->
	eval(L, [], []).

eval([], [Res], _L2) ->
	Res;
eval([')' | T], [H11, H12 | T1], [H2 | T2]) ->
	eval(T, [eval_do(H11, H12, H2) | T1], T2);
eval([H | T], L1, L2) when erlang:is_integer(H) ->
	eval(T, [H | L1], L2);
eval(['(' | T], L1, L2) ->
	eval(T, L1, L2);
eval([H | T], L1, L2) ->
	eval(T, L1, [H | L2]).



% ['(', 1, '+', '(', '(', 2, '+', 3, ')', '*', '(', 4, '*', 5, ')', ')', ')']

eval_do(H11, H12, '+') ->
	H12 + H11;
eval_do(H11, H12, '*') ->
	H12 * H11;
eval_do(H11, H12, '-') ->
	H12 - H11;
eval_do(H11, H12, '/') ->
	H12/H11.

threesum(L) ->
	threesum(L, 0).

threesum([], Res) ->
	Res;
threesum([_X1, X2, X3 | T], Res) ->
	threesum([X2, X3 | T], Res + 1);
threesum(_, Res) ->
	Res.