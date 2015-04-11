-module(rabbit_client_sup).

-export([start_link/2]).
-export([init/1]).


start_link(SupName, CallBack) ->
	supervisor:start_link(SupName, ?MODULE, CallBack).

init({M, F, A}) ->
	{ok, {
		  {simple_one_for_one, 0, 1},
		  [{client, {M, F, A}, temporary, infinity, supervisor, [M]}]
	}}.