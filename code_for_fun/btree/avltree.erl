-module(avltree).

% -export([ new/0
%         , new/1
%         , insert/2
%         ]).
% -export([ mid/1
%         , prev/1
%         , last/1
%         ]).

-compile(export_all).

-define(undef, undefined).
-define(empty_tree,
        {node, ?undef, ?undef, ?undef, ?undef, 0}).
-define(only_root(Root, RootValue),
        {node, Root, ?undef, ?undef, RootValue, 1}).
-define(only_root(Root, RootValue, Height),
        {node, Root, ?undef, ?undef, RootValue, Height}).
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
    update_tree({node, Root, ?only_root(Key, Value), Right, RootValue, 0}, Key, left);
insert({node, Root, Left, Right, RootValue, _}, {Key, Value}) when Key < Root ->
    update_tree({node, Root, insert(Left, {Key, Value}), Right, RootValue, 0}, Key, left);
insert(?right_undefined(Root, Left, RootValue, _), {Key, Value}) when Key > Root ->
    update_tree({node, Root, Left, ?only_root(Key, Value), RootValue, 0}, Key, right);
insert({node, Root, Left, Right, RootValue, _}, {Key, Value}) when Key > Root ->
    update_tree({node, Root, Left, insert(Right, {Key, Value}), RootValue, 0}, Key, right).

deletemin(?empty_tree = T) ->
    T;
deletemin(?left_undefined(_, Right, _, _)) ->
    Right;
%% left's left is ?undef
deletemin({node, Root, ?left_undefined(_, LeftRight, _, _), Right, RootValue, _}) ->
    update_tree({node, Root, LeftRight, Right, RootValue, 0}, delete_left);
deletemin({node, Root, Left, Right, RootValue, _}) ->
    update_tree({node, Root, deletemin(Left), Right, RootValue, 0}, delete_left).

delete(?empty_tree = T, _) ->
    T;
delete(?undef, _) ->
    ?undef;
%% delete left, left has no children
delete({node, Root, ?only_root(Key, _, _), Right, RootValue, _}, Key) ->
    update_height(?left_undefined(Root, Right, RootValue, 0));
%% delete right, right has no children
delete({node, Root, Left, ?only_root(Key, _, _), RootValue, _}, Key) ->
    update_height(?right_undefined(Root, Left, RootValue, 0));
%% delete root, root has no right child
delete(?right_undefined(Root, Left, _, _), Root) ->
    update_height(Left);
delete({node, Root, Left, Right, _, _}, Root) ->
    {NewRoot, NewRootValue} = get_min(Right),
    update_tree({node, NewRoot, Left, deletemin(Right), NewRootValue, 0}, delete_left);
delete({node, Root, Left, Right, RootValue, _}, Key) when Key > Root ->
    update_tree({node, Root, Left, delete(Right, Key), RootValue, 0}, delete_right);
delete({node, Root, Left, Right, RootValue, _}, Key) when Key < Root ->
    update_tree({node, Root, delete(Left, Key), Right, RootValue, 0}, delete_left).

get_min(?left_undefined(Root, _, RootValue, _)) ->
    {Root, RootValue};
get_min({node, _, Left, _, _, _}) ->
    get_min(Left).

update_tree({node, _, Left, {node, _, RightLeft, RightRight, _, _} = Right, _, _} = Tree, delete_left) ->
    case height(Right) - height(Left) == 2 of
        true ->
            case height(RightLeft) > height(RightRight) of
                true ->
                    right_left_rotation(Tree);
                false ->
                    right_right_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end;
update_tree({node, _, {node, _, LeftLeft, LeftRight, _, _} = Left, Right, _, _} = Tree, delete_right) ->
    case height(Left) - height(Right) == 2 of
        true ->
            case height(LeftRight) > height(LeftLeft) of
                true ->
                    left_right_rotation(Tree);
                false ->
                    left_left_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end.

update_tree({node, _, {node, LeftRoot, _, _, _, _} = Left, Right, _, _} = Tree,
            Key, left) ->
    case height(Left) - height(Right) >= 2 of
        true ->
            if
                Key < LeftRoot ->
                    left_left_rotation(Tree);
                true ->
                    left_right_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end;
update_tree({node, _, Left, {node, RightRoot, _, _, _, _} = Right, _, _} = Tree,
            Key, right) ->
    case height(Right) - height(Left) >= 2 of
        true ->
            if
                Key > RightRoot ->
                    right_right_rotation(Tree);
                true ->
                    right_left_rotation(Tree)
            end;
        false ->
            update_height(Tree)
    end.

left_left_rotation(OldTree) ->
    {node, OldTreeRoot, OldTreeLeft, OldTreeRight, OldTreeRootValue, _} = OldTree,
    {node, NewTreeRoot, NewTreeLeft, TreeRight, NewTreeRootValue, _} = OldTreeLeft,
    NewTreeRight = {node, OldTreeRoot, TreeRight, OldTreeRight, OldTreeRootValue, 0},
    update_height({node, NewTreeRoot, NewTreeLeft,
                   update_height(NewTreeRight), NewTreeRootValue, 0}).

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

update_height({node, _, Left, Right, _, _} = Tree) ->
    Height = erlang:max(height(Left), height(Right)) + 1,
    erlang:setelement(6, Tree, Height).

height(?undef) ->
    0;
height({node, _, _, _, _, Height}) ->
    Height.

% height(?undef) ->
%     0;
% height(?only_root(_, _, _)) ->
%     1;
% height(?right_undefined(_, Left, _, _)) ->
%     height(Left) + 1;
% height(?left_undefined(_, Right, _, _)) ->
%     height(Right) + 1;
% height({node, _, Left, Value, _, _}) ->
%     erlang:max(height(Left), height(Value)) + 1.

prev(?undef) ->
    [];
prev(?only_root(Value, _, _)) ->
    [Value];
prev(?right_undefined(Value, Left, _, _)) ->
    [Value | prev(Left)];
prev(?left_undefined(Value, Right, _, _)) ->
    [Value | prev(Right)];
prev({node, Value, Left, Right, _, _}) ->
    [Value] ++ prev(Left) ++ prev(Right).

mid(?undef) ->
    [];
mid(?only_root(Value, _, _)) ->
    [Value];
mid(?right_undefined(Value, Left, _, _)) ->
    mid(Left) ++ [Value];
mid(?left_undefined(Value, Right, _, _)) ->
    [Value | mid(Right)];
mid({node, Value, Left, Right, _, _}) ->
    mid(Left) ++ [Value] ++ mid(Right).

last(?undef) ->
    [];
last(?only_root(Value, _, _)) ->
    [Value];
last(?right_undefined(Value, Left, _, _)) ->
    last(Left) ++ [Value];
last(?left_undefined(Value, Right, _, _)) ->
    last(Right) ++ [Value];
last({node, Value, Left, Right, _, _}) ->
    last(Left) ++ last(Right) ++ [Value].

is_avl(?empty_tree) ->
    true;
is_avl(?undef) ->
    true;
is_avl({node, _Root, Left, Right, _, _}) ->
    erlang:abs(height(Left) - height(Right)) =< 1
        andalso is_avl(Left) andalso is_avl(Right).
    % case erlang:abs(height(Left) - height(Right)) =< 1 of
    %     false ->
    %         false;
    %     _ ->
    %         is_avl(Left) andalso is_avl(Right)
    %         % case is_avl(Left) of
    %         %     true ->
    %         %         is_avl(Right);
    %         %     false ->
    %         %         false
    %         % end
    % end.

test() ->
    A = [{3,a},
         {2,a},
         {1,a},
         {4,a},
         {5,a},
         {6,a},
         {7,a},
         {16,a},
         {15,a},
         {14,a},
         {13,a},
         {12,a},
         {11,a},
         {10,a},
         {8,a},
         {9,a}],
    Tree = new(A),
    [7,4,2,1,3,6,5,13,11,9,8,10,12,15,14,16] = prev(Tree),
    [1,3,2,5,6,4,8,10,9,12,11,14,16,15,13,7] = last(Tree),
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] = mid(Tree),
    true.
