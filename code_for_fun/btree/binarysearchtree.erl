
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

-record(node, {root, left, right, rootvalue}).

-define(undef, undefined).
-define(empty_tree,
        #node{ root = ?undef
             , left = ?undef, right = ?undef, rootvalue = ?undef}).

new() ->
    ?empty_tree.

new(KVList) ->
    lists:foldl(fun({Key, Value}, Acc) -> insert(Acc, {Key, Value}) end,
                new(), KVList).

insert(?undef, {Key, Value}) ->
    #node{root = Key, rootvalue = Value};
insert(?empty_tree, {Key, Value}) ->
    #node{root = Key, rootvalue = Value};
insert(#node{root = Root, left = Left,
             right = Right} = OldTree, {Key, Value}) ->
    if
        Key == Root ->
            OldTree#node{rootvalue = Value};
        Key > Root ->
            OldTree#node{right = insert(Right, {Key, Value})};
        Key < Root ->
            OldTree#node{left = insert(Left, {Key, Value})}
    end.

look(?undef, _) ->
    [];
look(?empty_tree, _) ->
    [];
look(#node{ root = Root
          , left = Left
          , right = Right
          , rootvalue = RootValue}, Key) ->
    if
        Key == Root ->
            [{Key, RootValue}];
        Key > Root ->
            look(Right, Key);
        Key < Root ->
            look(Left, Key)
    end.

min(#node{root = Root, left = ?undef}) ->
    Root;
min(#node{left = Left}) ->
    min(Left).

max(#node{root = Root, right = ?undef}) ->
    Root;
max(#node{right = Right}) ->
    max(Right).

rank(?undef) -> 0;
rank(?empty_tree) -> 0;
rank(#node{left = Left, right = Right}) ->
    1 + rank(Left) + rank(Right).

rank(?undef, _) -> 0;
rank(?empty_tree, _) -> 0;
rank(#node{root = Root, left = Left, right = Right}, Key) ->
    if
        Key == Root ->
            1 + rank(Left);
        Key > Root ->
            1 + rank(Left, Key) + rank(Right, Key);
        Key < Root ->
            rank(Left, Key)
    end.

deletemin(?empty_tree) -> ?empty_tree;
deletemin(#node{left = ?undef, right = Right}) ->
    Right;
deletemin(#node{left = Left} = OldTree) ->
    case ?undef == Left#node.left of
        true ->
            %% found min node
            OldTree#node{left = Left#node.right};
        _ ->
            OldTree#node{left = deletemin(Left)}
    end.

deletemax(?empty_tree) -> ?empty_tree;
deletemax(#node{right = ?undef, left = Left}) ->
    Left;
deletemax(#node{right = Right} = OldTree) ->
    case ?undef == Right#node.right of
        true ->
            %% found max node
            OldTree#node{right = Right#node.left};
        _ ->
            OldTree#node{right = deletemax(Right)}
    end.

delete(T, _) when T == ?empty_tree; T == ?undef -> T;
delete(#node{root = Root, left = Left, right = Right} = OldTree, Key) ->
    if
        Key == Root ->
            case Right of
                ?undef -> Left;
                _      ->
                    {NewRoot, NewRootValue} = get_min(Right),
                    OldTree#node{ root = NewRoot
                                , right = deletemin(Right)
                                , rootvalue = NewRootValue}
            end;
        Key > Root ->
            OldTree#node{right = delete(Right, Key)};
        Key < Root ->
            OldTree#node{left = delete(Left, Key)}
    end.

get_min(#node{root = Root, left = ?undef, rootvalue = RootValue}) ->
    {Root, RootValue};
get_min(#node{left = Left}) ->
    get_min(Left).

%% == prev
floor(?empty_tree, _) -> null;
floor(?undef, _)      -> null;
floor(#node{root = Root, left = Left,
            right = Right, rootvalue = RootValue}, Key) ->
    if
        Key == Root ->
            {Root, RootValue};
        Key < Root ->
            floor(Left, Key);
        Key > Root andalso Right == ?undef ->
            {Root, RootValue};
        Key > Root ->
            case min(Right) > Key of
                true ->
                    {Root, RootValue};
                false ->
                    floor(Right, Key)
            end
    end.

ceiling(?empty_tree, _) -> null;
ceiling(?undef, _)      -> null;
ceiling(#node{root = Root, left = Left,
              right = Right, rootvalue = RootValue}, Key) ->
    if
        Key == Root ->
            {Root, RootValue};
        Key > Root ->
            ceiling(Right, Key);
        Key < Root andalso Left == ?undef ->
            {Root, RootValue};
        Key < Root ->
            case max(Left) < Key of
                true ->
                    {Root, RootValue};
                false ->
                    ceiling(Left, Key)
            end
    end.

prev(?undef) -> [];
prev(#node{root = Root, left = ?undef, right = ?undef}) ->
    [Root];
prev(#node{root = Root, left = ?undef, right = Right}) ->
    [Root | prev(Right)];
prev(#node{root = Root, left = Left, right = ?undef}) ->
    [Root | prev(Left)];
prev(#node{root = Root, left = Left, right = Right}) ->
    [Root] ++ prev(Left) ++ prev(Right).

mid(?undef) -> [];
mid(#node{root = Root, left = Left, right = Right}) ->
    case {Left, Right} of
        {?undef, ?undef} ->
            [Root];
        {?undef, _} ->
            [Root | mid(Right)];
        {_, ?undef} ->
            mid(Left) ++ [Root];
        _ ->
            mid(Left) ++ [Root] ++ mid(Right)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    A = binarysearchtree:new([{10, 1}, {4, 1}, {5, 1}, {8, 1},
             {11, 1}, {17, 1}, {14, 1}, {19, 1}]),
    [4,5,8,10,11,14,17,19] = mid(A),
    1   = rank(A, 4),
    2   = rank(A, 5),
    3   = rank(A, 8),
    4   = rank(A, 10),
    5   = rank(A, 11),
    6   = rank(A, 14),
    7   = rank(A, 17),
    8   = rank(A, 19),
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
    null    = floor(A, 1),
    {4 , 1} = floor(A, 4),
    {5 , 1} = floor(A, 6),
    {11, 1} = floor(A, 12),
    {19, 1} = floor(A, 20),
    {4 , 1} = ceiling(A, 1),
    {10, 1} = ceiling(A, 9),
    {19, 1} = ceiling(A, 18),
    null    = ceiling(A, 20),
    [{10, 1}] = look(A, 10),
    [{4 , 1}] = look(A, 4),
    [{5 , 1}] = look(A, 5),
    [{8 , 1}] = look(A, 8),
    [{14, 1}] = look(A, 14),
    [] = look(A, 111111),
    [] = look(A, 7),
    [] = look(A, -1),
    true.
