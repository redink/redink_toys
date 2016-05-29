-module(tree).

-export([prev/1, mid/1, last/1]).
-export([from_last_mid/2]).
-export([from_prev_mid/2]).
-export([bfs/1]).

prev({node, Value, undefined, undefined}) ->
    [Value];
prev({node, Value, Left, undefined}) ->
    [Value | prev(Left)];
prev({node, Value, undefined, Right}) ->
    [Value | prev(Right)];
prev({node, Value, Left, Right}) ->
    [Value] ++ prev(Left) ++ prev(Right).

mid({node, Value, undefined, undefined}) ->
    [Value];
mid({node, Value, Left, undefined}) ->
    mid(Left) ++ [Value];
mid({node, Value, undefined, Right}) ->
    [Value | mid(Right)];
mid({node, Value, Left, Right}) ->
    mid(Left) ++ [Value] ++ mid(Right).

last({node, Value, undefined, undefined}) ->
    [Value];
last({node, Value, Left, undefined}) ->
    last(Left) ++ [Value];
last({node, Value, undefined, Right}) ->
    last(Right) ++ [Value];
last({node, Value, Left, Right}) ->
    last(Left) ++ last(Right) ++ [Value].

from_last_mid(_, []) ->
    undefined;
from_last_mid([], _) ->
    undefined;
from_last_mid(Last, Mid) ->
    {LastHead, [Root]} = lists:split(erlang:length(Last) - 1, Last),
    case spliton(Root, Mid) of
        {Left, Right} ->
            {node, Root, from_last_mid(LastHead, Left),
                         from_last_mid(LastHead, Right)};
        not_found ->
            from_last_mid(LastHead, Mid)
    end.

from_prev_mid(_, []) ->
    undefined;
from_prev_mid([], _)->
    undefined;
from_prev_mid(Prev, Mid) ->
    [Root | PrevTail] = Prev,
    case spliton(Root, Mid) of
        {Left, Right} ->
            {node, Root, from_prev_mid(PrevTail, Left),
                         from_prev_mid(PrevTail, Right)};
        not_found ->
            from_prev_mid(PrevTail, Mid)
    end.


bfs(Node) ->
    lists:flatten(internal_bfs(Node)).

internal_bfs({node, Value, undefined, undefined}) ->
    [[Value]];
internal_bfs({node, Value, Left, undefined}) ->
    [[Value] | internal_bfs(Left)];
internal_bfs({node, Value, undefined, Right}) ->
    [[Value] | internal_bfs(Right)];
internal_bfs({node, Value, Left, Right}) ->
    LLeft  = internal_bfs(Left),
    LRight = internal_bfs(Right),
    [[Value] | zip_lists_of_lists(LLeft, LRight)].

zip_lists_of_lists(LL1, []) -> LL1;
zip_lists_of_lists([], LL2) -> LL2;
zip_lists_of_lists(LL1, LL2) ->
    [H1 | Tail1] = LL1,
    [H2 | Tail2] = LL2,
    Head =
        case {H1, H2} of
            {L1, []} -> L1;
            {[], L2} -> L2;
            {L1, L2} -> L1 ++ L2
        end,
    [Head | zip_lists_of_lists(Tail1, Tail2)].

spliton(Elem, List) ->
    {Before, From} = lists:splitwith(fun(A) -> A /= Elem end, List),
    case From of
        [Elem | After] ->
            {Before, After};
        _ ->
            not_found
    end.
