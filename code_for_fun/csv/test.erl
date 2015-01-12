-module(test).
-export([start/0]).

-include("csv.hrl").

-record(test, {id, title, data}).


make_result(make, Test) ->
    Opts = proplists:get_value(opts, Test#test.data),
    Data = proplists:get_value(data, Test#test.data),
    Res = csv:make(Opts, Data),
    io_lib:format("~w", [Res]);
make_result(put_frecs, Test) ->
    File = Test#test.id ++ ".csv",
    Opts = proplists:get_value(opts, Test#test.data),
    Data = proplists:get_value(data, Test#test.data),
    ok = csv:put_frecs(Opts, Data, File, [write]),
    Res = file:read_file(File),
    Ret = io_lib:format("~w", [Res]),
    Ret;
make_result(parse, Test) ->
    File = Test#test.id ++ ".csv",
    Opts = proplists:get_value(opts, Test#test.data),
    Res = csv:get_frecs(Opts, File),
    io_lib:format("~w", [Res]).


do_test([], _Num, Passed_num, _F) ->
    Passed_num;

do_test([H|T], Num, Passed_num, F_report_start) ->
    F_report_start(Num, H#test.title),
    Result = io_lib:format("~w", [proplists:get_value(ret, H#test.data)]),
    Output = make_result(proplists:get_value(act, H#test.data), H),
    case Result =:= Output of
	true ->
	    report_status("ok"),
	    Passed_num_new = Passed_num + 1;
	false ->
	    report_status("fail"),
	    Passed_num_new = Passed_num
    end,
    do_test(T, Num + 1, Passed_num_new, F_report_start).


make_report_fun(T) ->
    Len = length(T),
    Title_maxlen = lists:max(lists:map(fun(A) -> length(A#test.title) end, T)),
    Io_str = "~w/~w: ~-" ++ integer_to_list(Title_maxlen) ++ "...s ",
    fun (Num, Title) ->
	    io:format(Io_str, [Num, Len, Title])
    end.


report_status(Status) ->
    io:format("~s~n", [Status]).


start() ->
    T = [#test{id="m01",
	       title="Make: [\"test\", \"rows\", \"here\"]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, ["test", "rows", "here"]},
		     {ret, {ok, "test,rows,here\n"}}]},
	 #test{id="m02",
	       title="Make: [\"test\\\"\", \"rows\", \"here\"]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, ["test\"", "rows", "here"]},
		     {ret, {ok, "\"test\"\"\",rows,here\n"}}]},
	 #test{id="m03",
	       title="Make: [\"test\\\"\", \"ro,ws\", \"here\"]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, ["test\"", "ro,ws", "here"]},
		     {ret, {ok, "\"test\"\"\",\"ro,ws\",here\n"}}]},
	 #test{id="m04",
	       title="Make: [\"test\\\"\", \"ro,ws\", \"he\\nre\"]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, ["test\"", "ro,ws", "he\nre"]},
		     {ret, {ok, "\"test\"\"\",\"ro,ws\",\"he\nre\"\n"}}]},
	 #test{id="m05",
	       title="Make: [\"test\", 1, 2]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, ["test", 1, 2]},
		     {ret, {ok, "test,1,2\n"}}]},
	 #test{id="m06",
	       title="Make: [\"test\", 1, 2.0]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, ["test", 1, 2.0]},
		     {ret, {ok, "test,1,2.00000000000000000000e+00\n"}}]},
	 #test{id="m07",
	       title="Make: [\"test\", 1, 2.0](with sep=\".\")",
	       data=[{act, make},
		     {opts, #csv{sep = "."}},
		     {data, ["test", 1, 2.0]},
		     {ret, {ok, "test.1.\"2.00000000000000000000e+00\"\n"}}]},
	 #test{id="m08",
	       title="Make: {\"test\", 1, 2.0} (with cb)",
	       data=[{act, make},
		     {opts,
		      #csv{cb_make = fun(_O, R) -> {ok, tuple_to_list(R)} end}},
		     {data, {"test", 1, 2.0}},
		     {ret, {ok, "test,1,2.00000000000000000000e+00\n"}}]},
	 #test{id="m09",
	       title="Make: {my_rec, \"test\", 1, 2.0} (with cb)",
	       data=[{act, make},
		     {opts,
		      #csv{cb_make = fun(_O, R) -> csv:cb_rec_to_list(R) end}},
		     {data, {my_rec, "test", 1, 2.0}},
		     {ret, {ok, "test,1,2.00000000000000000000e+00\n"}}]},
	 #test{id="m10",
	       title="Make: [atom_here, \"test\", 1]",
	       data=[{act, make},
		     {opts, #csv{}},
		     {data, [atom_here, "test", 1]},
		     {ret, {ok, "atom_here,test,1\n"}}]},
	 #test{id="m11",
	       title="Make: put_frecs: one row",
	       data=[{act, put_frecs},
		     {opts, #csv{}},
		     {data, [[atom_here, "test", 1, 123], [atom_here2, "test2", 2, 1234]]},
		     {ret, {ok, <<"atom_here,test,1\n">>}}]},
	 #test{id="m12",
	       title="Make: put_frecs: two rows",
	       data=[{act, put_frecs},
		     {opts, #csv{}},
		     {data, [[atom_here, "test", 1], ["here", 1, "lines"]]},
		     {ret, {ok, <<"atom_here,test,1\nhere,1,lines\n">>}}]},
	 #test{id="p01",
	       title="Parse: single field file",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["single field"]]}}]},
	 #test{id="p02",
	       title="Parse: single field file(without newline)",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["single field without newline"]]}}]},
	 #test{id="p03",
	       title="Parse: two simple recs",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["simple","rows","here"],
				 ["and","one","else"]]}}]},
	 #test{id="p04",
	       title="Parse: two quoted recs",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["text","is","quoted"],
				 ["and","here","too"]]}}]},
	 #test{id="p05",
	       title="Parse: field with commas",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["field,with,commas","here"],
				 ["and","another","rec"]]}}]},
	 #test{id="p06",
	       title="Parse: field with commas and quotes",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["field,with,commas","and\"with\"quotes"],
				 ["and","another","rec","1","2","3"]]}}]},
	 #test{id="p07",
	       title="Parse: field with quotes at start",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","\"quotes\"at\"start"],
				 ["and","another"]]}}]},
	 #test{id="p08",
	       title="Parse: field with quotes at end",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","quotes\"at\"end\""],
				 ["and","another"]]}}]},
	 #test{id="p09",
	       title="Parse: field with quotes at both ends",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","\"quotes\"at\"both ends\""],
				 ["and","another"]]}}]},
	 #test{id="p10",
	       title="Parse: field with only quotes 1",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,",""],
				 ["and","another"]]}}]},
	 #test{id="p11",
	       title="Parse: field with only quotes 2",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","\""],
				 ["and","another"]]}}]},
	 #test{id="p12",
	       title="Parse: field with only quotes 3",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","\"\""],
				 ["and","another"]]}}]},
	 #test{id="p13",
	       title="Parse: field with newline",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","text\non two lines"],
				 ["and","another"]]}}]},
	 #test{id="p14",
	       title="Parse: field with newline and empty field 1",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","text\non two lines",""],
				 ["and","another"]]}}]},
	 #test{id="p15",
	       title="Parse: field with newline and empty field 2",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["comma,","text\non two lines","",""],
				 ["and","another"]]}}]},
	 #test{id="p16",
	       title="Parse: field with newline and empty field 3",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [["","comma,","text\non two lines","",""],
				 ["and","another"]]}}]},
	 #test{id="p17",
	       title="Parse: no field",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, []}}]},
	 #test{id="p18",
	       title="Parse: empty field",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {ok, [[""]]}}]},
	 #test{id="p19",
	       title="Parse: quote in an unquoted field",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {error,[{error,"Quote in an unquoted field"},
				   {at_line,1}]}}]},
	 #test{id="p20",
	       title="Parse: not ended field",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {error,[{error,want_next_line},{at_line,2}]}}]},
	 #test{id="p21",
	       title="Parse: unescaped quote",
	       data=[{act, parse},
		     {opts, #csv{}},
		     {ret, {error,[{error,"Quote is unescaped in the field"},
				   {at_line,1}]}}]},
	 #test{id="p22",
	       title="Parse: field with newline and empty field 3 (sep=\";\", quote=\"[]\")",
	       data=[{act, parse},
		     {opts, #csv{sep=";", quote="[]"}},
		     {ret, {ok, [["","comma;","text\non two lines","",""],
				 ["and","another"]]}}]},
	 #test{id="p23",
	       title="Parse: field with only quotes 3 (sep=\";\", quote=\"[]\")",
	       data=[{act, parse},
		     {opts, #csv{sep=";", quote="[]"}},
		     {ret, {ok, [["comma;","[][]"],
				 ["and","another"]]}}]}
	],
    Report_fun = make_report_fun(T),
    Passed_num = do_test(T, 1, 0, Report_fun),
    io:format("~nPassed: ~w/~w~n", [Passed_num, length(T)]).