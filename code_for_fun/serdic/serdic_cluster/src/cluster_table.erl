-module(cluster_table).

-export([mnesia/1]).

mnesia(boot) ->
    ok = cluster_mnesia:create_table(person,
                                     [ {type, set}
                                     , {disc_copies, [node()]}
                                     , {attributes, [name, age]}
                                     ]),
    ok;
mnesia(copy) ->
    ok = cluster_mnesia:copy_table(person, disc_copies),
    ok.
