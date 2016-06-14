
-module(rktree).

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
        , is_binarysearchtree/1
        ]).

-export([test/0]).

-record(node, {root, left, right, rootvalue, csize = 0}).

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
    #node{root = Key, rootvalue = Value, csize = 0};
insert(?empty_tree, {Key, Value}) ->
    #node{root = Key, rootvalue = Value, csize = 0};
insert(#node{root = Root, left = Left,
             right = Right,
             csize = CSize} = OldTree, {Key, Value}) ->
    if
        Key == Root ->
            OldTree#node{rootvalue = Value};
        Key > Root ->
            OldTree#node{right = insert(Right, {Key, Value}),
                         csize = CSize + 1};
        Key < Root ->
            OldTree#node{left = insert(Left, {Key, Value}),
                         csize = CSize + 1}
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

rank(T, _) when T == ?empty_tree; T == ?undef -> null;
rank(#node{root = Root, left = ?undef, right = Right}, Key) ->
    if
        Key == Root ->
            1;
        Key > Root ->
            1 + rank(Right, Key);
        Key < Root ->
            null
    end;
rank(#node{root = Root, left = Left, right = Right}, Key) ->
    if
        Key == Root ->
            Left#node.csize + 1 + 1;
        Key > Root ->
            Left#node.csize + 1 + 1 + rank(Right, Key);
        Key < Root ->
            rank(Left, Key)
    end.

deletemin(?empty_tree) -> ?empty_tree;
deletemin(#node{left = ?undef, right = Right}) ->
    Right;
deletemin(#node{left = Left, csize = CSize} = OldTree) ->
    case ?undef == Left#node.left of
        true ->
            %% found min node
            OldTree#node{left = Left#node.right,
                         csize = CSize - 1};
        _ ->
            OldTree#node{left = deletemin(Left),
                         csize = CSize - 1}
    end.

deletemax(?empty_tree) -> ?empty_tree;
deletemax(#node{right = ?undef, left = Left}) ->
    Left;
deletemax(#node{right = Right, csize = CSize} = OldTree) ->
    case ?undef == Right#node.right of
        true ->
            %% found max node
            OldTree#node{right = Right#node.left, csize = CSize - 1};
        _ ->
            OldTree#node{right = deletemax(Right), csize = CSize - 1}
    end.

delete(T, _) when T == ?empty_tree; T == ?undef -> T;
delete(#node{root = Root, left = Left, right = Right,
             csize = CSize} = OldTree, Key) ->
    if
        Key == Root ->
            case Right of
                ?undef -> Left;
                _      ->
                    {NewRoot, NewRootValue} = get_min(Right),
                    OldTree#node{ root = NewRoot
                                , right = deletemin(Right)
                                , rootvalue = NewRootValue
                                , csize = CSize - 1}
            end;
        Key > Root ->
            OldTree#node{right = delete(Right, Key),
                         csize = CSize - 1};
        Key < Root ->
            OldTree#node{left = delete(Left, Key),
                         csize = CSize - 1}
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

mid_rank(T) ->
    [rank(T, X) || X <- mid(T)].

% is_sorted([]) ->
%     true;
% is_sorted([A, B | T]) ->
%     A =< B andalso is_sorted([B | T]).

is_binarysearchtree(T) when T == ?undef; T == ?empty_tree -> true;
is_binarysearchtree(#node{left = Left,
                          right = Right, root = Root}) ->
    case {Left, Right} of
        {?undef, ?undef} ->
            true;
        {?undef, _} ->
            Root < Right#node.root andalso is_binarysearchtree(Right);
        {_, ?undef} ->
            Root > Left#node.root andalso is_binarysearchtree(Left);
        _ ->
            Root > Left#node.root andalso Root < Right#node.root
                andalso is_binarysearchtree(Left) andalso is_binarysearchtree(Right)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    A = rktree:new([{10, 1}, {4, 1}, {5, 1}, {8, 1},
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
    true = is_binarysearchtree(A1),
    true = lists:seq(1, 7) == mid_rank(A1),
    A2  = deletemin(A1 ), [8,10,11,14,17,19]  = mid(A2),
    true = is_binarysearchtree(A2),
    true = lists:seq(1, 6) == mid_rank(A2),
    A3  = deletemin(A2 ), [10,11,14,17,19] = mid(A3),
    true = is_binarysearchtree(A3),
    true = lists:seq(1, 5) == mid_rank(A3),
    A4  = deletemin(A3 ), [11,14,17,19] = mid(A4),
    true = is_binarysearchtree(A4),
    true = lists:seq(1, 4) == mid_rank(A4),
    A5  = deletemin(A4 ), [14,17,19] = mid(A5),
    true = is_binarysearchtree(A5),
    true = lists:seq(1, 3) == mid_rank(A5),
    A6  = deletemin(A5 ), [17,19] = mid(A6),
    true = is_binarysearchtree(A6),
    true = lists:seq(1, 2) == mid_rank(A6),
    A7  = deletemin(A6 ), [19] = mid(A7),
    true = is_binarysearchtree(A7),
    true = lists:seq(1, 1) == mid_rank(A7),
    A8  = deletemin(A7 ), [] = mid(A8),
    true = is_binarysearchtree(A8),
    A01 = deletemax(A  ), [4,5,8,10,11,14,17] = mid(A01), true = is_binarysearchtree(A01),
    A02 = deletemax(A01), [4,5,8,10,11,14] = mid(A02), true = is_binarysearchtree(A02),
    A03 = deletemax(A02), [4,5,8,10,11] = mid(A03), true = is_binarysearchtree(A03),
    A04 = deletemax(A03), [4,5,8,10] = mid(A04), true = is_binarysearchtree(A04),
    A05 = deletemax(A04), [4,5,8] = mid(A05), true = is_binarysearchtree(A05),
    A06 = deletemax(A05), [4,5] = mid(A06), true = is_binarysearchtree(A06),
    A07 = deletemax(A06), [4] = mid(A07), true = is_binarysearchtree(A07),
    A08 = deletemax(A07), [] = mid(A08), true = is_binarysearchtree(A08),
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
    [true = lists:seq(1, 7) == mid_rank(delete(A, X))
     || X <- [4,5,8,10,11,14,17,19]],
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
