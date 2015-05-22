-ifndef(INNER_PB_H).
-define(INNER_PB_H, true).
-record(inner, {
    camelcase
}).
-endif.

-ifndef(OUTER_PB_H).
-define(OUTER_PB_H, true).
-record(outer, {
    camelcase,
    innerlist = []
}).
-endif.

