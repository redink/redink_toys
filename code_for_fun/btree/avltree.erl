-module(avltree).

-export([ new/0
        , new/1
        , insert/2
        ]).

-define(undef, undefined).
-define(empty_tree,
        {node, ?undef, ?undef, ?undef, ?undef, 0}).
-define(only_root(Root, RootValue),
        {node, Root, ?undef, ?undef, RootValue, 1}).
-define(left_undefined(Root, Right, RootValue, Height),
        {node, Root, ?undef, Right, RootValue, Height}).
-define(right_undefined(Root, Left, RootValue, Height),
        {node, Root, Left, ?undef, RootValue, Height}).

new() ->
    {node, undefined, undefined, undefined, undefined, 0}.

new(KVList) ->
    lists:foldl(fun({Key, Value}, Acc) -> insert(Acc, {Key, Value}) end,
                new(), KVList).

insert(?empty_tree, {Key, Value}) ->
    ?only_root(Key, Value);
insert({node, Root, Left, Right, _, Height}, {Root, Value}) ->
    {node, Root, Left, Right, Value, Height};
insert(?left_undefined(Root, Right, RootValue, _), {Key, Value}) when Key < Root ->
    T = {node, Root, ?only_root(Key, Value), Right, RootValue, 0},
    update_tree(T, left);
insert({node, Root, Left, Right, RootValue, _}, {Key, Value}) when Key < Root ->
    T = {node, Root, insert(Left, {Key, Value}), Right, RootValue, 0},
    update_tree(T, left);
insert(?right_undefined(Root, Left, RootValue, _), {Key, Value}) when Key > Root ->
    T = {node, Root, Left, ?only_root(Key, Value), RootValue, 0},
    update_tree(T, right);
insert({node, Root, Left, Right, RootValue, _}, {Key, Value}) when Key > Root ->
    update_tree({node, Root, Left, insert(Right, {Key, Value}), RootValue, 0}, right).

height(undefined) ->
    0;
height({node, _, undefined, undefined, _, _}) ->
    1;
height({node, _, Left, undefined, _, _}) ->
    height(Left) + 1;
height({node, _, undefined, Right, _, _}) ->
    height(Right) + 1;
height({node, _, Left, Value, _, _}) ->
    erlang:max(height(Left), height(Value)) + 1.

update_tree({node, Root, {node, LeftRoot, _, _, _, _} = Left, Right, _, _} = Tree, left) ->
    LeftHeight  = height(Left),
    RightHeight = height(Right),
    case LeftHeight - RightHeight >= 2 of
        true ->
            if
                Root < LeftRoot ->
                    left_left_rotation(Tree);
                true ->
                    left_right_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end;
update_tree({node, Root, Left, {node, RightRoot, _, _, _, _} = Right, _, _} = Tree, right) ->
    case height(Right) - height(Left) >= 2 of
        true ->
            if
                Root > RightRoot ->
                    right_right_rotation(Tree);
                true ->
                    right_left_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end.

update_height(Tree) ->
    erlang:setelement(6, Tree, height(Tree)).

left_left_rotation({node, _, undefined, _, _, _} = OldTree) ->
    OldTree;
left_left_rotation(OldTree) ->
    {node, OldTreeRoot, OldTreeLeft, OldTreeRight, OldTreeRootValue, _} = OldTree,
    {node, NewTreeRoot, NewTreeLeft, TreeRight, NewTreeRootValue, _} = OldTreeLeft,
    NewTreeRight = {node, OldTreeRoot, TreeRight, OldTreeRight, OldTreeRootValue, 0},
    update_height({node, NewTreeRoot, NewTreeLeft,
                   update_height(NewTreeRight), NewTreeRootValue, 0}).

right_right_rotation({node, _, _, undefined, _, _} = OldTree) ->
    OldTree;
right_right_rotation(OldTree) ->
    {node, OldTreeRoot, OldTreeLeft, OldTreeRight, OldTreeRootValue, _} = OldTree,
    {node, NewTreeRoot, TreeLeft, NewTreeRight, NewTreeRootValue, _} = OldTreeRight,
    NewTreeLeft = {node, OldTreeRoot, OldTreeLeft, TreeLeft, OldTreeRootValue, 0},
    update_height({node, NewTreeRoot, update_height(NewTreeLeft),
                   NewTreeRight, NewTreeRootValue, 0}).

right_left_rotation({_, _, _, OldRight, _, _} = OldTree) ->
    NewRight = left_left_rotation(OldRight),
    right_right_rotation(erlang:setelement(4, OldTree, NewRight)).

left_right_rotation({_, _, OldLeft, _, _, _} = OldTree) ->
    NewLeft = right_right_rotation(OldLeft),
    left_left_rotation(erlang:setelement(3, OldTree, NewLeft)).
