-module(append_test).

-compile(export_all).

'test++'() ->
    [1] ++ [2,3].
'test++'(A) ->
    [A] ++ [2,3].
'test++'(A, List) ->
    [A] ++ List.

'test|'() ->
    [1, 2, 3].
'test|'(A) ->
    [A, 2, 3].
'test|'(A, L) ->
    [A | L].

'test++++'(A, L) ->
    A ++ L.