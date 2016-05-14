-module(monmgr_table_def).

-export([mnesia/1]).

mnesia(boot) ->
    ok = cluster_mnesia:create_table(timezone_mapping,
                                     [ {type, set}
                                     , {disc_copies, [node()]}
                                     , {attributes, [timezone, groups]}
                                     ]),
    ok = cluster_mnesia:create_table(node_mapping,
                                     [ {type, bag}
                                     , {disc_copies, [node()]}
                                     , {attributes, [group_name, nodes]}
                                     ]),
    ok = cluster_mnesia:create_table(group_set,
                                     [ {type, set}
                                     , {disc_copies, [node()]}
                                     , {attributes, [group_name, status]}
                                     ]),
    ok;
mnesia(copy) ->
    ok = cluster_mnesia:copy_table(timezone_mapping, disc_copies),
    ok = cluster_mnesia:copy_table(node_mapping    , disc_copies),
    ok = cluster_mnesia:copy_table(group_set       , disc_copies),
    ok.
