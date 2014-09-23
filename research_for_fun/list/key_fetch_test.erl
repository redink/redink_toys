-module(key_fetch_test).

-export ([main/1,
		 test_keyfind/2,
		 test_proplist/2]).

main(N) ->
	TestData = mk_test_data(N),

	{TimeKeyFind, _} = timer:tc(?MODULE, test_keyfind, [N*10, TestData]),
	{TimePorpList, _} = timer:tc(?MODULE, test_proplist, [N*10, TestData]),

	{TimeKeyFind, TimePorpList}.


mk_test_data(N) ->
	[{X, X} || X <- lists:seq(1, N)].

test_keyfind(0, _) ->
	ok;

test_keyfind(N, TestData) ->
	lists:keyfind(N, 1, TestData),
	test_keyfind(N - 1, TestData).

test_proplist(0, _) ->
	ok;

test_proplist(N, TestData) ->
	proplists:get_value(N, TestData),
	test_proplist(N - 1, TestData).