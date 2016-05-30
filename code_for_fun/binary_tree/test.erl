-module(test).

-export([start/1, f/1]).

start(A) ->
	case A of
		1 -> 1;
		2 -> 2;
		3 -> 3;
		4 -> 4;
		_ -> 0
	end.

f(A) ->
	end1(A).

end1(1) -> 1;
end1(2) -> 2;
end1(3) -> 3;
end1(4) -> 4;
end1(_) -> 0.

	