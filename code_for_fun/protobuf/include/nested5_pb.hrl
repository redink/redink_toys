-ifndef(FIRST_PB_H).
-define(FIRST_PB_H, true).
-record(first, {
    inner
}).
-endif.

-ifndef(SECOND_PB_H).
-define(SECOND_PB_H, true).
-record(second, {
    inner = erlang:error({required, inner})
}).
-endif.

-ifndef(FIRST_INNER_PB_H).
-define(FIRST_INNER_PB_H, true).
-record(first_inner, {
    foo
}).
-endif.

