#!/usr/bin/env escript

main(_) ->
    
    compile:file("./to_date_test.erl"),
    
    code:add_patha("./"),

    io:format("----- N = 10,       the compare result is ~p ~n", [to_date_test:to_date(10)]),

    io:format("----- N = 100,      the compare result is ~p ~n", [to_date_test:to_date(100)]),

    io:format("----- N = 1000,     the compare result is ~p ~n", [to_date_test:to_date(1000)]),

    io:format("----- N = 10000,    the compare result is ~p ~n", [to_date_test:to_date(10000)]),

    io:format("----- N = 100000,   the compare result is ~p ~n", [to_date_test:to_date(100000)]),

    io:format("----- N = 1000000,  the compare result is ~p ~n", [to_date_test:to_date(1000000)]),

    io:format("----- N = 10000000, the compare result is ~p ~n", [to_date_test:to_date(10000000)]),

    os:cmd("rm -rf to_date_test.beam").