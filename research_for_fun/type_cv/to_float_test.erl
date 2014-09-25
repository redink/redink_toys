-module(to_float_test).

-compile(export_all).

to_float(N) ->
	TestSource = mk_test_data(N),

	{TO, _} = timer:tc(?MODULE, to_float_old_list, [TestSource]),
	{TN, _} = timer:tc(?MODULE, to_float_new_list, [TestSource]),

	{TO, TN}.

mk_test_data(N) ->
	[<<"0.11">> || _ <- lists:seq(1, N)].

to_float_old_list([]) ->
	ok;
to_float_old_list([H | T]) ->
	to_float_old(H),
	to_float_old_list(T).


to_float_new_list([]) ->
	ok;
to_float_new_list([H | T]) ->
	to_float_new(H),
	to_float_new_list(T).

to_float_old(Data) ->
	{ok, [Num], _Leftovers} = case io_lib:fread("~f", binary_to_list(Data)) of
                                           % note: does not need conversion
        {error, _} ->
          case io_lib:fread("~d", binary_to_list(Data)) of  % note: does not need conversion
            {ok, [_], []} = Res ->
              Res;
            {ok, [X], E} ->
              io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s" ,[X,".0",E])))
          end
        ;
        Res ->
          Res
    end,
    Num.

to_float_new(Data) ->
	case catch erlang:binary_to_float(Data) of
		A when erlang:is_float(A)->
			A;
		{'EXIT', {badarg, _}} ->
			case catch erlang:binary_to_integer(Data) of
				R when erlang:is_integer(R) ->
					R;
				_ ->
					to_float_old(Data)
			end;
		_ ->
			to_float_old(Data)
	end.

