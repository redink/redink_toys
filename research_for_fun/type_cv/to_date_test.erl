-module (to_date_test).

-export ([to_date/1,
          to_date_old/1,
          to_date_new/1,
          to_date_han/1,
          to_date_old_list/1,
          to_date_new_list/1,
          to_date_han_list/1]).


to_date_old(Data) ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Data)) of  % note: does not need conversion
        {ok, [Year, Month, Day], _} ->
            {date, {Year, Month, Day}};
        {error, _} ->
            binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, bad_date})
    end.

to_date_new(Data) ->
    try
        [Y, M, D] = binary:split(Data, [<<"-">>], [global]),
        {date, {erlang:binary_to_integer(Y),
                erlang:binary_to_integer(M),
                erlang:binary_to_integer(D)}} of
            R ->
                R
    catch 
        _: _ ->
            to_date_old(Data)
    end.

to_date_han(Data) ->
    try
        {date, {erlang:binary_to_integer(binary:part(Data, {0, 4})),
                erlang:binary_to_integer(binary:part(Data, {5, 2})),
                erlang:binary_to_integer(binary:part(Data, {8, 2}))}} of
            R ->
                R
    catch
        _ : _ ->
            to_date_old(Data)
    end.


to_date(N) ->
    A = mk_test_source(N),

    {TOld, _} = timer:tc(?MODULE, to_date_old_list, [A]),

    {TNew, _} = timer:tc(?MODULE, to_date_new_list, [A]),

    {THan, _} = timer:tc(?MODULE, to_date_han_list, [A]),

    {TOld, TNew, THan}.

to_date_old_list([]) ->
    ok;
to_date_old_list([H | T]) ->
    to_date_old(H),
    to_date_old_list(T).

to_date_new_list([]) ->
    ok;
to_date_new_list([H | T]) ->
    to_date_new(H),
    to_date_new_list(T).

to_date_han_list([]) ->
    ok;
to_date_han_list([H | T]) ->
    to_date_han(H),
    to_date_han_list(T).

mk_test_source(N) ->
    [<<"2014-06-01">> || _ <- lists:seq(1, N)].
