#!/usr/bin/env escript

main(_) ->
    
    compile:file("./to_float_test.erl"),
    
    code:add_patha("./"),

    io:format("----- N = 10,       the compare result is ~p ~n", [to_float_test:to_float(10)]),

    io:format("----- N = 100,      the compare result is ~p ~n", [to_float_test:to_float(100)]),

    io:format("----- N = 1000,     the compare result is ~p ~n", [to_float_test:to_float(1000)]),

    io:format("----- N = 10000,    the compare result is ~p ~n", [to_float_test:to_float(10000)]),

    io:format("----- N = 100000,   the compare result is ~p ~n", [to_float_test:to_float(100000)]),

    io:format("----- N = 1000000,  the compare result is ~p ~n", [to_float_test:to_float(1000000)]),

    io:format("----- N = 10000000, the compare result is ~p ~n", [to_float_test:to_float(10000000)]),

    os:cmd("rm -rf to_float_test.beam").