
-module(serdic_cluster).

-export([start/0]).
-export([ join/1
        , leave/0
        , status/0
        , remove/1
        ]).

start() ->
    {ok, _} = application:ensure_all_started(serdic_cluster),
    ok.

join(Node) when Node =:= erlang:node() ->
    {error, {cannot_join_self, Node}};
join(Node) when erlang:is_atom(Node) ->
    case {is_running(Node), is_clustered(Node)} of
        {true, false} ->
            ok = cluster_mnesia:join_cluster(Node);
        {false, false} ->
            {error, {node_not_runuing, Node}};
        {_, true} ->
            {error, {already_clustered, Node}}
    end.

leave() ->
    case lists:member(erlang:node(), cluster_mnesia:running_nodes()) of
        true ->
            ok = cluster_mnesia:leave_cluster();
        _ ->
            {error, node_not_in_cluster}
    end.

remove(Node) when Node =:= erlang:node() ->
    {error, {cannot_remove_self, Node}};
remove(Node) when erlang:is_atom(Node) ->
    cluster_mnesia:remove_from_cluster(Node).

status() ->
    cluster_mnesia:cluster_status().

-spec is_running(atom()) -> boolean().
is_running(Node) ->
    case catch rpc:call(Node, erlang, whereis, [?MODULE]) of
        Pid when erlang:is_pid(Pid) ->
            true;
        _ ->
            false
    end.

-spec is_clustered(atom()) -> boolean().
is_clustered(Node) ->
    lists:member(Node, cluster_mnesia:running_nodes()).

