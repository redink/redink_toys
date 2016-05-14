-module(monmgr_wr).

-export([ mnesia_read/2
        , mnesia_keys/1
        , mnesia_write/2
        , delete_object/2
        , delete_key/2
        ]).

mnesia_read(Table, Key) ->
    mnesia:dirty_read(Table, Key).
    
mnesia_keys(Table) ->
    mnesia:dirty_all_keys(Table).

mnesia_write(Table, Object) ->
    mnesia:dirty_write(Table, Object).

delete_object(Table, Object) ->
    mnesia:dirty_delete_object(Table, Object).

delete_key(Table, Key) ->
    mnesia:dirty_delete(Table, Key).
