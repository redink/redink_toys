#!/usr/bin/env escript

main(_) ->
    
    compile:file("./hibernate_test.erl"),
    
    code:add_patha("../../deps/recon/ebin"),
    code:add_patha("./"),

    {ok, Pid} = hibernate_test:start_link(),

    {memory, HB} = recon:info(Pid, memory),

    timer:sleep(11000),

    {memory, HA} = recon:info(Pid, memory),
    
    os:cmd("rm hibernate_test.beam"),
    
    io:format("before hibernate process memory: ~p, after memory: ~p ~n", [HB, HA]),

    {HB, HA}.