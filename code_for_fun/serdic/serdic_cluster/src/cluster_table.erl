-module(cluster_table).

-export([mnesia/1]).

mnesia(boot) ->
    ok = cluster_mnesia:create_table(person,
                                     [ {type, set}
                                     , {disc_copies, [node()]}
                                     , {attributes, [name, age]}
                                     ]),
    ok = cluster_mnesia:create_table(node_type,
                                     [ {type, set}
                                     , {ram_copies, [node()]}
                                     , {attributes, [node_name, type, time]}
                                     ]),
    ok;
mnesia(copy) ->
    ok = cluster_mnesia:copy_table(person, disc_copies),
    ok = cluster_mnesia:copy_table(node_type, ram_copies),
    ok.
