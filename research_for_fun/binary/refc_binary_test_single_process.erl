-module(refc_binary_test_single_process).

-export ([start/0,
          handle_big_binary/0]).

start() ->
    C = handle_big_binary(),
    io:format("---- handled binary size ~p~n", [erlang:byte_size(C)]),
    io:format("---- handled binary ref size ~p~n", [binary:referenced_byte_size(C)]),
    io:format("------- handled ~p~n", [erts_debug:get_internal_state({binary_info, C})]),
    timer:sleep(1000000).
    
handle_big_binary() ->
    A = binary:copy(<<1>>, 1024*1024),
    io:format("-------- resource ~p~n", [erts_debug:get_internal_state({binary_info, A})]),
    <<B:1/binary, _/binary>> = A,
    binary:copy(B).

    %% B.

%% erts_debug:set_internal_state(available_internal_state, true).
%% proc binary：{refc_binary, pb_size, {binary, orig_size}, pb_flags}