-ifndef(OUTER_PB_H).
-define(OUTER_PB_H, true).
-record(outer, {
    middleaa,
    middlebb
}).
-endif.

-ifndef(OUTER_MIDDLEBB_PB_H).
-define(OUTER_MIDDLEBB_PB_H, true).
-record(outer_middlebb, {
    inner
}).
-endif.

-ifndef(OUTER_MIDDLEAA_PB_H).
-define(OUTER_MIDDLEAA_PB_H, true).
-record(outer_middleaa, {
    inner
}).
-endif.

-ifndef(OUTER_MIDDLEBB_INNER_PB_H).
-define(OUTER_MIDDLEBB_INNER_PB_H, true).
-record(outer_middlebb_inner, {
    ival = erlang:error({required, ival}),
    booly
}).
-endif.

-ifndef(OUTER_MIDDLEAA_INNER_PB_H).
-define(OUTER_MIDDLEAA_INNER_PB_H, true).
-record(outer_middleaa_inner, {
    ival = erlang:error({required, ival}),
    booly
}).
-endif.

