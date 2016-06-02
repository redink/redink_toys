
-module(binarysearchtree).

-export([ new/0
        , new/1
        , prev/1
        , mid/1
        , insert/2
        , look/2
        , min/1
        , max/1
        , rank/2
        , deletemin/1
        , deletemax/1
        , delete/2
        , floor/2
        , ceiling/2
        ]).

-export([test/0]).

new() ->
    {node, undefined, undefined, undefined, undefined}.

new(KVList) ->
    lists:foldl(fun({Key, Value}, Acc) -> insert(Acc, {Key, Value}) end,
                new(), KVList).

%% empty
insert({node, undefined, undefined, undefined, undefined}, {Key, Value}) ->
    {node, Key, undefined, undefined, Value};
%% equal root
insert({node, Root, Left, Right, _}, {Root, Value}) ->
    {node, Root, Left, Right, Value};
%% < root
%% left is undefined
insert({node, Root, undefined, Right, RootValue}, {Key, Value}) when Key < Root ->
    {node, Root, {node, Key, undefined, undefined, Value}, Right, RootValue};
insert({node, Root, Left, Right, RootValue}, {Key, Value}) when Key < Root ->
    {node, Root, insert(Left, {Key, Value}), Right, RootValue};
%% > root
%% right is undefined
insert({node, Root, Left, undefined, RootValue}, {Key, Value}) when Key > Root ->
    {node, Root, Left, {node, Key, undefined, undefined, Value}, RootValue};
insert({node, Root, Left, Right, RootValue}, {Key, Value}) when Key > Root ->
    {node, Root, Left, insert(Right, {Key, Value}), RootValue}.


%% empty
look({node, undefined, undefined, undefined, undefined}, _) ->
    [];
%% == root
look({node, Root, _, _, RootValue}, Root) ->
    [{Root, RootValue}];
%% < root
look({node, Root, undefined, _, _}, Key) when Key < Root ->
    [];
look({node, Root, Left, _, _}, Key) when Key < Root ->
    look(Left, Key);
%% > root
look({node, Root, _, undefined, _}, Key) when Key > Root ->
    [];
look({node, Root, _, Right, _}, Key) when Key > Root ->
    look(Right, Key).

min({node, Root, undefined, _, _}) ->
    Root;
min({node, _, Left, _, _}) ->
    min(Left).

max({node, Root, _, undefined, _}) ->
    Root;
max({node, _, _, Right, _}) ->
    max(Right).

%% empty
rank({node, undefined, undefined, undefined, undefined}, _) ->
    0;
rank(undefined, _) ->
    0;
%% left is empty
rank({node, Root, undefined, _, _}, Root) ->
    1;
rank({node, Root, Left, _, _}, Root) ->
    1 + rank(Left);
%% < root
rank({node, Root, undefined, _, _}, Key) when Key < Root ->
    0;
rank({node, Root, Left, _, _}, Key) when Key < Root ->
    rank(Left, Key);
%% > root
rank({node, Root, Left, undefined, _}, Key) when Key > Root ->
    1 + rank(Left, Key);
rank({node, Root, Left, Right, _}, Key) when Key > Root ->
    1 + rank(Left, Key) + rank(Right, Key).

rank(undefined) ->
    0;
rank({node, undefined, undefined, undefined, undefined}) ->
    0;
rank({node, _, Left, Right, _}) ->
    1 + rank(Left) + rank(Right).

%% empty
deletemin({node, undefined, undefined, undefined, undefined} = T) ->
    T;
%% left is empty
deletemin({node, _, undefined, Right, _}) ->
    Right;
%% left's left is empty
deletemin({node, Root, {node, _, undefined, LeftRight, _}, Right, RootValue}) ->
    {node, Root, LeftRight, Right, RootValue};
deletemin({node, Root, Left, Right, RootValue}) ->
    {node, Root, deletemin(Left), Right, RootValue}.

%% empty
deletemax({node, undefined, undefined, undefined, undefined} = T) ->
    T;
%% right is empty
deletemax({node, _, Left, undefined, _}) ->
    Left;
deletemax({node, Root, Left, {node, _, RightLeft, undefined, _}, RootValue}) ->
    {node, Root, Left, RightLeft, RootValue};
deletemax({node, Root, Left, Right, RootValue}) ->
    {node, Root, Left, deletemax(Right), RootValue}.

%% empty
delete({node, undefined, undefined, undefined, undefined} = T, _) ->
    T;
delete(undefined, _) ->
    undefined;
%% delete left, left has no children
delete({node, Root, {node, Key, undefined, undefined, _}, Right, RootValue}, Key) ->
    {node, Root, undefined, Right, RootValue};
%% delete right, right has no children
delete({node, Root, Left, {node, Key, undefined, undefined, _}, RootValue}, Key) ->
    {node, Root, Left, undefined, RootValue};
%% delete root, root has no right child
delete({node, Root, Left, undefined, _}, Root) ->
    Left;
delete({node, Root, Left, Right, _}, Root) ->
    {NewRoot, NewRootValue} = get_min(Right),
    {node, NewRoot, Left, deletemin(Right), NewRootValue};
delete({node, Root, Left, Right, RootValue}, Key) when Key > Root ->
    {node, Root, Left, delete(Right, Key), RootValue};
delete({node, Root, Left, Right, RootValue}, Key) when Key < Root ->
    {node, Root, delete(Left, Key), Right, RootValue}.

get_min({node, Root, undefined, _, RootValue}) ->
    {Root, RootValue};
get_min({node, _, Left, _, _}) ->
    get_min(Left).

floor({node, undefined, undefined, undefined, undefined}, _) ->
    null;
%% only one node
floor({node, Root, undefined, undefined, RootValue}, _) ->
    {Root, RootValue};
%% root == key
floor({node, Root, _, _, RootValue}, Root) ->
    {Root, RootValue};
%% left is undefined
floor({node, Root, undefined, _, RootValue}, Key) when Key < Root ->
    {Root, RootValue};
floor({node, Root, Left, _, _RootValue}, Key) when Key < Root ->
    floor(Left, Key);
%% right is undefined
floor({node, Root, _, undefined, RootValue}, Key) when Key > Root ->
    {Root, RootValue};
floor({node, Root, _, Right, RootValue}, Key) when Key > Root ->
    case min(Right) =< Key of
        false ->
            {Root, RootValue};
        true ->
            floor(Right, Key)
    end.

ceiling({node, undefined, undefined, undefined, undefined}, _) ->
    null;
ceiling({node, Root, undefined, undefined, RootValue}, _) ->
    {Root, RootValue};
ceiling({node, Root, _, _, RootValue}, Root) ->
    {Root, RootValue};
%% right is undefined
ceiling({node, Root, _, undefined, RootValue}, Key) when Key > Root ->
    {Root, RootValue};
ceiling({node, Root, _, Right, _RootValue}, Key) when Key > Root ->
    ceiling(Right, Key);
%% left is undefined
ceiling({node, Root, undefined, _, RootValue}, Key) when Key < Root ->
    {Root, RootValue};
ceiling({node, Root, Left, _, RootValue}, Key) when Key < Root ->
    case max(Left) >= Key of
        false ->
            {Root, RootValue};
        true ->
            ceiling(Left, Key)
    end.

prev({node, Value, undefined, undefined, _}) ->
    [Value];
prev({node, Value, Left, undefined, _}) ->
    [Value | prev(Left)];
prev({node, Value, undefined, Right, _}) ->
    [Value | prev(Right)];
prev({node, Value, Left, Right, _}) ->
    [Value] ++ prev(Left) ++ prev(Right).

mid(undefined) ->
    [];
mid({node, Value, undefined, undefined, _}) ->
    [Value];
mid({node, Value, Left, undefined, _}) ->
    mid(Left) ++ [Value];
mid({node, Value, undefined, Right, _}) ->
    [Value | mid(Right)];
mid({node, Value, Left, Right, _}) ->
    mid(Left) ++ [Value] ++ mid(Right).


test() ->
    A = new([{10, 1}, {4, 1}, {5, 1}, {8, 1},
             {11, 1}, {17, 1}, {14, 1}, {19, 1}]),
    [4,5,8,10,11,14,17,19] = mid(A),
    A1 = deletemin(A ), [5,8,10,11,14,17,19] = mid(A1),
    A2 = deletemin(A1), [8,10,11,14,17,19]  = mid(A2),
    A3 = deletemin(A2), [10,11,14,17,19] = mid(A3),
    A4 = deletemin(A3), [11,14,17,19] = mid(A4),
    A5 = deletemin(A4), [14,17,19] = mid(A5),
    A6 = deletemin(A5), [17,19] = mid(A6),
    A7 = deletemin(A6), [19] = mid(A7),
    A8 = deletemin(A7), [] = mid(A8),
    A01 = deletemax(A  ), [4,5,8,10,11,14,17] = mid(A01),
    A02 = deletemax(A01), [4,5,8,10,11,14] = mid(A02),
    A03 = deletemax(A02), [4,5,8,10,11] = mid(A03),
    A04 = deletemax(A03), [4,5,8,10] = mid(A04),
    A05 = deletemax(A04), [4,5,8] = mid(A05),
    A06 = deletemax(A05), [4,5] = mid(A06),
    A07 = deletemax(A06), [4] = mid(A07),
    A08 = deletemax(A07), [] = mid(A08),
    4  = min(A ),
    5  = min(A1),
    8  = min(A2),
    10 = min(A3),
    11 = min(A4),
    14 = min(A5),
    17 = min(A6),
    19 = min(A7),
    19 = max(A  ),
    17 = max(A01),
    14 = max(A02),
    11 = max(A03),
    10 = max(A04),
    8  = max(A05),
    5  = max(A06),
    4  = max(A07),
    [5,8,10,11,14,17,19] = mid(delete(A, 4)),
    [4,8,10,11,14,17,19] = mid(delete(A, 5)),
    [4,5,10,11,14,17,19] = mid(delete(A, 8)),
    [4,5,8,11,14,17,19] = mid(delete(A, 10)),
    [4,5,8,10,14,17,19] = mid(delete(A, 11)),
    [4,5,8,10,11,17,19] = mid(delete(A, 14)),
    [4,5,8,10,11,14,19] = mid(delete(A, 17)),
    [4,5,8,10,11,14,17] = mid(delete(A, 19)),
    {4, 1} = floor(A, 1),
    {4, 1} = floor(A, 4),
    {5, 1} = floor(A, 6),
    {11, 1} = floor(A, 12),
    {19, 1} = floor(A, 20),
    {4, 1} = ceiling(A, 1),
    {10, 1} = ceiling(A, 9),
    {19, 1} = ceiling(A, 18),
    [] = look(A, 7),
    [{10, 1}] = look(A, 10),
    [{4 , 1}] = look(A, 4),
    [{5 , 1}] = look(A, 5),
    [{8 , 1}] = look(A, 8),
    [{14, 1}] = look(A, 14),
    [] = look(A, 111111),
    [] = look(A, -1),
    true.