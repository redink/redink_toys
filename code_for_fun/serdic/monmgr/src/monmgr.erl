-module(monmgr).

-export([ start/0
        , register/2
        , unregister/2
        , add_group/1
        , delete_group/1
        , delete_group/2
        , get_nodelist/3
        , get_nodelist/4
        ]).

start() ->
    {ok, _} = application:ensure_all_started(monmgr),
    ok.

register(GroupName, NodeName) ->
    ok = monmgr_wr:mnesia_write(node_mapping,
                                {node_mapping, GroupName, NodeName}),
    ok = add_group(GroupName).

unregister(GroupName, NodeName) ->
    ok = monmgr_wr:delete_object(node_mapping,
                                 {node_mapping, GroupName, NodeName}).

add_group(GroupName) ->
    ok = monmgr_wr:mnesia_write(group_set, {group_set, GroupName, 1}).

delete_group(GroupName) ->
    delete_group(GroupName, false).

delete_group(GroupName, Force) ->
    case monmgr_wr:mnesia_read(group_set, GroupName) of
        [] ->
            ok;
        _ ->
            delete_group_do(GroupName, Force)
    end.                    

get_nodelist(TimeZone, YMD, PartKey) ->
    case monmgr_wr:mnesia_read(timezone_mapping, {TimeZone, YMD}) of
        [] ->
            NewGroupList = monmgr_wr:mnesia_keys(group_set),
            R = get_partkey(NewGroupList, PartKey),
            monmgr_wr:mnesia_write(timezone_mapping,
                                   {timezone_mapping, {TimeZone, YMD}, NewGroupList}),
            R;
        [{_, _, GroupList}] ->
            get_partkey(GroupList, PartKey)
    end.

get_nodelist(TimeZone, YMD, PartKey, {delete_old_ymd, OldYMD}) ->
    R  = get_nodelist(TimeZone, YMD, PartKey),
    ok = delete_old_timezone_info(TimeZone, OldYMD),
    R.

%%%%%%%%%%%%

get_partkey(GroupList0, PartKey) ->
    GroupList = lists:usort(GroupList0),
    GroupLen  = erlang:length(GroupList),
    GroupName = lists:nth(PartKey div (960 div GroupLen) + 1, GroupList),
    [Node || {_, _, Node} <- monmgr_wr:mnesia_read(node_mapping, GroupName)].

delete_group_do(GroupName, Force) ->
    case {monmgr_wr:mnesia_read(node_mapping, GroupName), Force} of
        {[], _} ->
            monmgr_wr:delete_key(group_set, GroupName),
            ok;
        {NodeList, true} ->
            [monmgr_wr:delete_object(node_mapping, X) || X <- NodeList],
            monmgr_wr:delete_key(group_set, GroupName),
            ok;
        {_NodeList, false} ->
            {error, "group not empty"}
    end.

delete_old_timezone_info(TimeZone, OldYMD) ->
    F =
        fun() ->
            monmgr_wr:delete_key(timezone_mapping, {TimeZone, OldYMD})
        end,
    spawn(F),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

monmgr_test_() ->
    [{"whole flow",
        fun() ->
            true = code:add_pathz("../serdic_cluster/ebin"),
            ok = application:set_env(serdic_cluster, table_def_mod, monmgr_table_def),
            ok = monmgr:start(),
            ok = monmgr:register(g1, n100),
            ok = monmgr:register(g1, n101),
            ok = monmgr:register(g1, n102),
            ok = monmgr:register(g2, m100),
            ok = monmgr:register(g2, m101),
            ok = monmgr:register(g2, m102),
            [n100, n101, n102] = lists:usort(monmgr:get_nodelist(timezone1, 20160101, 100)),
            [m100, m101, m102] = lists:usort(monmgr:get_nodelist(timezone1, 20160101, 780)),
            ok = monmgr:register(g3, x100),
            ok = monmgr:register(g3, x101),
            ok = monmgr:register(g3, x102),
            [m100, m101, m102] = lists:usort(monmgr:get_nodelist(timezone1, 20160101, 780)),
            [x100, x101, x102] = lists:usort(monmgr:get_nodelist(timezone1, 20160102, 780))
        end
     }
    ].

-endif.
