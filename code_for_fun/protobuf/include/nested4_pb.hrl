-ifndef(OUTER_PB_H).
-define(OUTER_PB_H, true).
-record(outer, {
    middle
}).
-endif.

-ifndef(OUTER_MIDDLE_PB_H).
-define(OUTER_MIDDLE_PB_H, true).
-record(outer_middle, {
    inner,
    other
}).
-endif.

-ifndef(OUTER_OTHER_PB_H).
-define(OUTER_OTHER_PB_H, true).
-record(outer_other, {
    bar
}).
-endif.

-ifndef(OUTER_MIDDLE_INNER_PB_H).
-define(OUTER_MIDDLE_INNER_PB_H, true).
-record(outer_middle_inner, {
    foo
}).
-endif.

