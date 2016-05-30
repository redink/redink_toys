%% binary search tree
-module(c3).

-export([insert/2, new/0, new/1, look/2]).
-export([min/1, max/1, rank/2]).
-export([deletemin/1, deletemax/1]).
-export([delete/2]).
-export([prev/1, mid/1]).
-export([floor/2, ceiling/2]).

new() ->
    {node, undefined, undefined, undefined, undefined}.

new(KVList) ->
    lists:foldl(fun({Key, Value}, Acc) -> insert(Acc, {Key, Value}) end, new(), KVList).

insert({node, undefined, undefined, undefined, undefined}, {Key, Value}) ->
    {node, Key, undefined, undefined, Value};
insert({node, Root, undefined, undefined, RootValue}, {Key, Value}) when Key < Root ->
    {node, Root, {node, Key, undefined, undefined, Value}, undefined, RootValue};
insert({node, Root, undefined, undefined, RootValue}, {Key, Value}) when Key > Root ->
    {node, Root, undefined, {node, Key, undefined, undefined, Value}, RootValue};
insert(undefined, {Key, Value}) ->
    {node, Key, undefined, undefined, Value};

insert({node, Root, Left, Right, _}, {Key, Value}) when Key == Root ->
    {node, Root, Left, Right, Value};
insert({node, Root, Left, Right, RootValue}, {Key, Value}) when Key < Root ->
    {node, Root, insert(Left, {Key, Value}), Right, RootValue};
insert({node, Root, Left, Right, RootValue}, {Key, Value}) when Key > Root ->
    {node, Root, Left, insert(Right, {Key, Value}), RootValue}.


look(undefined, _) ->
    not_found;
look({node, Root, _, _, RootValue}, Root) ->
    RootValue;
look({node, Root, Left, _, _}, Key) when Key < Root ->
    look(Left, Key);
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

rank(undefined, _Key) ->
    0;
rank({node, Root, Left, _, _}, Root) ->
    1 + rank(Left);
rank({node, Root, Left, _, _}, Key) when Key < Root ->
    rank(Left, Key);
rank({node, Root, Left, Right, _}, Key) when Key > Root ->
    1 + rank(Left) + rank(Right, Key).


rank({node, undefined, undefined, undefined, undefined}) ->
    0;
rank(undefined) ->
    0;
rank({node, _Root, Left, Right, _}) ->
    rank(Left) + rank(Right) + 1.


deletemin({node, undefined, undefined, undefined, undefined} = Node) ->
    Node;
deletemin({node, _Root, undefined, Right, _}) ->
    Right;
deletemin({node, Root, {node, _Root0, undefined, Right0, _}, Right, RootValue}) ->
    {node, Root, Right0, Right, RootValue};
deletemin({node, Root, Left, Right, RootValue}) ->
    {node, Root, deletemin(Left), Right, RootValue}.

deletemax({node, undefined, undefined, undefined, undefined} = Node) ->
    Node;
deletemax({node, _Root, Left, undefined, _}) ->
    Left;
deletemax({node, Root, Left, {node, _Root0, Left0, undefined, _}, RootValue}) ->
    {node, Root, Left, Left0, RootValue};
deletemax({node, Root, Left, Right, RootValue}) ->
    {node, Root, Left, deletemax(Right), RootValue}.


delete({node, Root, {node, Key, undefined, undefined, _}, Right, RootValue}, Key) ->
    {node, Root, undefined, Right, RootValue};
delete({node, Root, Left, {node, Key, undefined, undefined, _}, RootValue}, Key) ->
    {node, Root, Left, undefined, RootValue};
delete({node, Root, Left, undefined, _}, Root) ->
    Left;
delete({node, Root, Left, Right, _RootValue}, Root) ->
    {NewRoot, NewRootValue} = get_root_right(Right),
    {node, NewRoot, Left, deletemin(Right), NewRootValue};
delete({node, Root, undefined, Right, RootValue}, Key) when Key < Root ->
    {node, Root, undefined, Right, RootValue};
delete({node, Root, Left, Right, RootValue}, Key) when Key < Root ->
    {node, Root, delete(Left, Key), Right, RootValue};
delete({node, Root, Left, undefined, RootValue}, Key) when Key > Root ->
    {node, Root, Left, undefined, RootValue};
delete({node, Root, Left, Right, RootValue}, Key) when Key > Root ->
    {node, Root, Left, delete(Right, Key), RootValue}.

get_root_right({node, Root, undefined, _Right, RootValue}) ->
    {Root, RootValue};
get_root_right({node, _Root, Left, _, _RootValue}) ->
    get_root_right(Left).

%%%%%%%%%%%%%%%%%%%%%
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

mid({node, Value, undefined, undefined, _}) ->
    [Value];
mid({node, Value, Left, undefined, _}) ->
    mid(Left) ++ [Value];
mid({node, Value, undefined, Right, _}) ->
    [Value | mid(Right)];
mid({node, Value, Left, Right, _}) ->
    mid(Left) ++ [Value] ++ mid(Right).

