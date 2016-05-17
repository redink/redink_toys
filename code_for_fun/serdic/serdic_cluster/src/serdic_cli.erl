-module(serdic_cli).

-export([cluster/1]).

-define(PRINT_MSG(Msg), io:format(Msg)).
-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT_CMD(Cmd, Descr), io:format("~-48s# ~s~n", [Cmd, Descr])).
-define(USAGE(CmdList), [?PRINT_CMD(Cmd, Descr) || {Cmd, Descr} <- CmdList]).

cluster(["join", SNode]) ->
    case serdic_cluster:join(parse_name(SNode)) of
        ok ->
            ?PRINT_MSG("Join the cluster successfully.~n"),
            cluster(["status"]);
        {error, Error} ->
            ?PRINT("Failed to join the cluster: ~p~n", [Error])
    end;

cluster(["leave"]) ->
    case serdic_cluster:leave() of
        ok ->
            ?PRINT_MSG("Leave the cluster successfully.~n"),
            cluster(["status"]);
        {error, Error} ->
            ?PRINT("Failed to leave the cluster: ~p~n", [Error])
    end;

cluster(["remove", SNode]) ->
    case serdic_cluster:remove(parse_name(SNode)) of
        ok ->
            ?PRINT_MSG("Remove the node from cluster successfully.~n"),
            cluster(["status"]);
        {error, Error} ->
            ?PRINT("Failed to remove the node from cluster: ~p~n", [Error])
    end;

cluster(["status"]) ->
    ?PRINT("Cluster status: ~p~n", [serdic_cluster:status()]);

cluster(_) ->
    ?USAGE([{"cluster join <Node>",  "Join the cluster"},
            {"cluster leave",        "Leave the cluster"},
            {"cluster remove <Node>","Remove the node from cluster"},
            {"cluster status",       "Cluster status"}]).


%% @doc Parse node name
-spec(parse_name(string()) -> atom()).
parse_name(Name) when is_list(Name) ->
    case string:tokens(Name, "@") of
        [_Node, _Host] -> list_to_atom(Name);
        _              -> with_host(Name)
    end.

with_host(Name) ->
    [_, Host] = string:tokens(atom_to_list(node()), "@"),
    list_to_atom(lists:concat([Name, "@", Host])).
