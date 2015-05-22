-ifndef(EXTENDABLE_PB_H).
-define(EXTENDABLE_PB_H, true).
-record(extendable, {
    '$extensions' = dict:new()
}).
-endif.

-ifndef(MAXTENDABLE_PB_H).
-define(MAXTENDABLE_PB_H, true).
-record(maxtendable, {
    '$extensions' = dict:new()
}).
-endif.

