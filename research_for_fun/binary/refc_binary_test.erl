-module(refc_binary_test).

-export ([start/0,
          handle_big_binary/1]).

start() ->
    Me = erlang:self(),
    erlang:spawn(?MODULE, handle_big_binary, [Me]),
    receive
        {ok, C} ->
            io:format("------- handled ~p~n", [erts_debug:get_internal_state({binary_info, C})]),
            timer:sleep(1000000),
            C;
        _ ->
            error
    after 10000 ->
            error
    end.

handle_big_binary(Me) ->
    A = binary:copy(<<1>>, 1024*1024),
    io:format("------- resource ~p~n", [erts_debug:get_internal_state({binary_info, A})]),
    <<B:1/binary, _/binary>> = A,
    erlang:send(Me, {ok, B}).

    %% erlang:send(Me, {ok, binary:copy(B)}).

%% erts_debug:set_internal_state(available_internal_state, true).
%% proc binary：{refc_binary, pb_size, {binary, orig_size}, pb_flags}