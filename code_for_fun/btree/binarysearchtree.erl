
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

-define(undef, undefnied).
-define(empty_tree,
        {node, ?undef, ?undef, ?undef, ?undef}).
-define(only_root(Root, RootValue),
        {node, Root, ?undef, ?undef, RootValue}).
-define(left_undefined(Root, Right, RootValue),
        {node, Root, ?undef, Right, RootValue}).
-define(right_undefined(Root, Left, RootValue),
        {node, Root, Left, ?undef, RootValue}).

new() ->
    ?empty_tree.

new(KVList) ->
    lists:foldl(fun({Key, Value}, Acc) -> insert(Acc, {Key, Value}) end,
                new(), KVList).

%% empty
insert(?empty_tree, {Key, Value}) ->
    ?only_root(Key, Value);
%% equal root
insert({node, Root, Left, Right, _}, {Root, Value}) ->
    {node, Root, Left, Right, Value};
%% < root
%% left is ?undef
insert(?left_undefined(Root, Right, RootValue), {Key, Value}) when Key < Root ->
% insert({node, Root, ?undef, Right, RootValue}, {Key, Value}) when Key < Root ->
    {node, Root, ?only_root(Key, Value), Right, RootValue};
insert({node, Root, Left, Right, RootValue}, {Key, Value}) when Key < Root ->
    {node, Root, insert(Left, {Key, Value}), Right, RootValue};
%% > root
%% right is ?undef
insert(?right_undefined(Root, Left, RootValue), {Key, Value}) when Key > Root ->
% insert({node, Root, Left, ?undef, RootValue}, {Key, Value}) when Key > Root ->
    {node, Root, Left, ?only_root(Key, Value), RootValue};
insert({node, Root, Left, Right, RootValue}, {Key, Value}) when Key > Root ->
    {node, Root, Left, insert(Right, {Key, Value}), RootValue}.


%% empty
look(?empty_tree, _) ->
    [];
%% == root
look({node, Root, _, _, RootValue}, Root) ->
    [{Root, RootValue}];
%% < root
look(?left_undefined(Root, _, _), Key) when Key < Root ->
    [];
look({node, Root, Left, _, _}, Key) when Key < Root ->
    look(Left, Key);
%% > root
look(?right_undefined(Root, _, _), Key) when Key > Root ->
    [];
look({node, Root, _, Right, _}, Key) when Key > Root ->
    look(Right, Key).

min(?left_undefined(Root, _, _)) ->
    Root;
min({node, _, Left, _, _}) ->
    min(Left).

max(?right_undefined(Root, _, _)) ->
    Root;
max({node, _, _, Right, _}) ->
    max(Right).

%% empty
rank(?empty_tree, _) ->
    0;
rank(?undef, _) ->
    0;
%% left is empty
rank(?left_undefined(Root, _, _), Root) ->
    1;
rank({node, Root, Left, _, _}, Root) ->
    1 + rank(Left);
%% < root
rank(?left_undefined(Root, _, _), Key) when Key < Root ->
    0;
rank({node, Root, Left, _, _}, Key) when Key < Root ->
    rank(Left, Key);
%% > root
rank(?right_undefined(Root, Left, _), Key) when Key > Root ->
    1 + rank(Left, Key);
rank({node, Root, Left, Right, _}, Key) when Key > Root ->
    1 + rank(Left, Key) + rank(Right, Key).

rank(?undef) ->
    0;
rank(?empty_tree) ->
    0;
rank({node, _, Left, Right, _}) ->
    1 + rank(Left) + rank(Right).

%% empty
deletemin(?empty_tree = T) ->
    T;
%% left is empty
deletemin(?left_undefined(_, Right, _)) ->
    Right;
%% left's left is empty
deletemin({node, Root, ?left_undefined(_, LeftRight, _), Right, RootValue}) ->
    {node, Root, LeftRight, Right, RootValue};
deletemin({node, Root, Left, Right, RootValue}) ->
    {node, Root, deletemin(Left), Right, RootValue}.

%% empty
deletemax(?empty_tree = T) ->
    T;
%% right is empty
deletemax(?right_undefined(_, Left, _)) ->
    Left;
deletemax({node, Root, Left, ?right_undefined(_, RightLeft, _), RootValue}) ->
    {node, Root, Left, RightLeft, RootValue};
deletemax({node, Root, Left, Right, RootValue}) ->
    {node, Root, Left, deletemax(Right), RootValue}.

%% empty
delete(?empty_tree = T, _) ->
    T;
delete(?undef, _) ->
    ?undef;
%% delete left, left has no children
delete({node, Root, ?only_root(Key, _), Right, RootValue}, Key) ->
    ?left_undefined(Root, Right, RootValue);
%% delete right, right has no children
delete({node, Root, Left, ?only_root(Key, _), RootValue}, Key) ->
    ?right_undefined(Root, Left, RootValue);
%% delete root, root has no right child
delete(?right_undefined(Root, Left, _), Root) ->
    Left;
delete({node, Root, Left, Right, _}, Root) ->
    {NewRoot, NewRootValue} = get_min(Right),
    {node, NewRoot, Left, deletemin(Right), NewRootValue};
delete({node, Root, Left, Right, RootValue}, Key) when Key > Root ->
    {node, Root, Left, delete(Right, Key), RootValue};
delete({node, Root, Left, Right, RootValue}, Key) when Key < Root ->
    {node, Root, delete(Left, Key), Right, RootValue}.

get_min(?left_undefined(Root, _, RootValue)) ->
    {Root, RootValue};
get_min({node, _, Left, _, _}) ->
    get_min(Left).

floor(?empty_tree, _) ->
    null;
%% only one node
floor(?only_root(Root, RootValue), _) ->
    {Root, RootValue};
%% root == key
floor({node, Root, _, _, RootValue}, Root) ->
    {Root, RootValue};
%% left is ?undef
floor(?left_undefined(Root, _, RootValue), Key) when Key < Root ->
    {Root, RootValue};
floor({node, Root, Left, _, _RootValue}, Key) when Key < Root ->
    floor(Left, Key);
%% right is ?undef
floor(?right_undefined(Root, _, RootValue), Key) when Key > Root ->
    {Root, RootValue};
floor({node, Root, _, Right, RootValue}, Key) when Key > Root ->
    case min(Right) =< Key of
        false ->
            {Root, RootValue};
        true ->
            floor(Right, Key)
    end.

ceiling(?empty_tree, _) ->
    null;
ceiling(?only_root(Root, RootValue), _) ->
    {Root, RootValue};
ceiling({node, Root, _, _, RootValue}, Root) ->
    {Root, RootValue};
%% right is ?undef
ceiling(?right_undefined(Root, _, RootValue), Key) when Key > Root ->
    {Root, RootValue};
ceiling({node, Root, _, Right, _RootValue}, Key) when Key > Root ->
    ceiling(Right, Key);
%% left is ?undef
ceiling(?left_undefined(Root, _, RootValue), Key) when Key < Root ->
    {Root, RootValue};
ceiling({node, Root, Left, _, RootValue}, Key) when Key < Root ->
    case max(Left) >= Key of
        false ->
            {Root, RootValue};
        true ->
            ceiling(Left, Key)
    end.

prev(?undef) ->
    [];
prev(?only_root(Value, _)) ->
    [Value];
prev(?right_undefined(Value, Left, _)) ->
    [Value | prev(Left)];
prev(?left_undefined(Value, Right, _)) ->
    [Value | prev(Right)];
prev({node, Value, Left, Right, _}) ->
    [Value] ++ prev(Left) ++ prev(Right).

mid(?undef) ->
    [];
mid(?only_root(Value, _)) ->
    [Value];
mid(?right_undefined(Value, Left, _)) ->
    mid(Left) ++ [Value];
mid(?left_undefined(Value, Right, _)) ->
    [Value | mid(Right)];
mid({node, Value, Left, Right, _}) ->
    mid(Left) ++ [Value] ++ mid(Right).


test() ->
    A = new([{10, 1}, {4, 1}, {5, 1}, {8, 1},
             {11, 1}, {17, 1}, {14, 1}, {19, 1}]),
    [4,5,8,10,11,14,17,19] = mid(A),
    A1  = deletemin(A  ), [5,8,10,11,14,17,19] = mid(A1),
    A2  = deletemin(A1 ), [8,10,11,14,17,19]  = mid(A2),
    A3  = deletemin(A2 ), [10,11,14,17,19] = mid(A3),
    A4  = deletemin(A3 ), [11,14,17,19] = mid(A4),
    A5  = deletemin(A4 ), [14,17,19] = mid(A5),
    A6  = deletemin(A5 ), [17,19] = mid(A6),
    A7  = deletemin(A6 ), [19] = mid(A7),
    A8  = deletemin(A7 ), [] = mid(A8),
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
    [5,8,10,11,14,17,19] = mid(delete(A, 4 )),
    [4,8,10,11,14,17,19] = mid(delete(A, 5 )),
    [4,5,10,11,14,17,19] = mid(delete(A, 8 )),
    [4,5,8,11,14,17,19]  = mid(delete(A, 10)),
    [4,5,8,10,14,17,19]  = mid(delete(A, 11)),
    [4,5,8,10,11,17,19]  = mid(delete(A, 14)),
    [4,5,8,10,11,14,19]  = mid(delete(A, 17)),
    [4,5,8,10,11,14,17]  = mid(delete(A, 19)),
    {4 , 1} = floor(A, 1),
    {4 , 1} = floor(A, 4),
    {5 , 1} = floor(A, 6),
    {11, 1} = floor(A, 12),
    {19, 1} = floor(A, 20),
    {4 , 1} = ceiling(A, 1),
    {10, 1} = ceiling(A, 9),
    {19, 1} = ceiling(A, 18),
    [{10, 1}] = look(A, 10),
    [{4 , 1}] = look(A, 4),
    [{5 , 1}] = look(A, 5),
    [{8 , 1}] = look(A, 8),
    [{14, 1}] = look(A, 14),
    [] = look(A, 111111),
    [] = look(A, 7),
    [] = look(A, -1),
    true.
