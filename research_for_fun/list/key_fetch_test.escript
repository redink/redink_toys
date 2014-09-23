#!/usr/bin/env escript

main(_) ->
    
    compile:file("./key_fetch_test.erl"),
    
    code:add_patha("./"),

    io:format("----- N = 10,    the compare result is ~p ~n", [key_fetch_test:main(10)]),

    io:format("----- N = 20,    the compare result is ~p ~n", [key_fetch_test:main(20)]),

    io:format("----- N = 50,    the compare result is ~p ~n", [key_fetch_test:main(50)]),

    io:format("----- N = 100,   the compare result is ~p ~n", [key_fetch_test:main(100)]),

    io:format("----- N = 200,   the compare result is ~p ~n", [key_fetch_test:main(200)]),

    io:format("----- N = 500,   the compare result is ~p ~n", [key_fetch_test:main(500)]),

    io:format("----- N = 1000,  the compare result is ~p ~n", [key_fetch_test:main(1000)]),

    io:format("----- N = 5000,  the compare result is ~p ~n", [key_fetch_test:main(5000)]),

    io:format("----- N = 10000, the compare result is ~p ~n", [key_fetch_test:main(10000)]),

    os:cmd("rm -rf key_fetch_test.beam").