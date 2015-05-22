-ifndef(WITHDEFAULT_PB_H).
-define(WITHDEFAULT_PB_H, true).
-record(withdefault, {
    real1 = erlang:error({required, real1}),
    real2 = erlang:error({required, real2}),
    int1 = erlang:error({required, int1}),
    int2 = erlang:error({required, int2}),
    int3 = erlang:error({required, int3}),
    int4 = erlang:error({required, int4}),
    int5 = 5,
    int6 = 6,
    int7 = 7,
    int8 = 8,
    int9 = 9,
    int10 = 10,
    val1 = true,
    str1 = "test",
    str2 = []
}).
-endif.

