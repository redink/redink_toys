%% @doc Comma-separated values manipulation library (rfc4180).
%% @headerfile "csv.hrl"
%%
-module(csv).

-export([make/2, put_rec/3, put_recs/3, put_frecs/3, put_frecs/4,
	 parse/2, get_rec/2, get_recs/2, get_frecs/2]).
-export([cb_rec_to_list/1, cb_list_to_rec/2]).

-include("csv.hrl").

%% state = fstart | fdata | fend | rend
-record(csv_state, {state = fstart, quote_fl = 0, 
		    line_in = "", rec_out = [], field = ""}).
%-type csv_state() :: #csv_state{}.

%-define(dbg(X, Y), error_logger:info_msg("*dbg ~p(~p): " X,
%					 [?MODULE, ?LINE | Y])).
-define(dbg(X,Y), true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type field() :: string() | integer() | atom().
-type rec() :: [field()] | tuple().

-spec make(csv(), rec()) -> {ok, string()} | {error, Reason::any()}.
%%
%% @doc Make a CSV text record from supplied fields.
%%
make(Opts, Rec) ->
    case call_cb(Opts#csv.cb_make, Opts, Rec) of
	{ok, Rec_new} ->
	    {ok, make(Opts, Rec_new, [])};
	Ret ->
	    Ret
    end.

make(Opts, [], Acc) ->
    Acc ++ Opts#csv.eor;
make(Opts, [H|T], Acc) ->
    F = mk_field(Opts, H),
    case Acc of
	[] ->
	    make(Opts, T, F);
	_ ->
	    make(Opts, T, Acc ++ Opts#csv.sep ++ F)
    end.


-spec put_rec(csv(), rec(), file:io_device()) -> ok | {error, Reason::any()}.
%%
%% @doc Put a CSV record into a Fio.
%%
put_rec(Opts, Rec, Fio) ->
    case make(Opts, Rec) of
	{ok, Str} ->
	    file:write(Fio, Str);
	Ret ->
	    Ret
    end.


-spec put_recs(csv(), Recs::[rec()], file:io_device()) -> ok |
							  {error, Reason::any()}.
%%
%% @doc Put all CSV records into a Fio.
%%
put_recs(_Opts, [], _Fio) ->
    ok;
put_recs(Opts, [Rec|T], Fio) ->
    case put_rec(Opts, Rec, Fio) of
	ok ->
	    put_recs(Opts, T, Fio);
	Ret ->
	    Ret
    end.


-spec put_frecs(csv(), [rec()], string()) -> ok | {error, Reason::any()}.
%%
%% @doc Put all CSV records into a file. Open a file with [append] modes.
%%
put_frecs(Opts, Recs, Fname) ->
    put_frecs(Opts, Recs, Fname, [append]).

-spec put_frecs(csv(), [rec()], string(), [atom()]) -> ok |
						       {error, Reason::any()}.
%%
%% @doc Put all CSV records into a file. Open a file with Fmodes modes.
%%
put_frecs(Opts, Recs, Fname, Fmodes) ->
    case file:open(Fname, Fmodes) of
	{ok, F} ->
	    Ret = put_recs(Opts, Recs, F),
	    file:close(F),
	    Ret;
	Ret ->
	    Ret
    end.


-spec parse(csv(), string()) -> {ok, [string()]} | {error, Reason::any()}.
%%
%% @doc Parse a CSV text record into fields.
%%
parse(Opts, Line) when is_list(Line) ->
    ?dbg("Start parsing ~ts (Opts: ~p)~n", [Line, Opts]),
    case parse(Opts, #csv_state{line_in = Line}) of
	{ok, Rec} ->
	    {ok, Rec};
	{error, Reason, _State} ->
	    {error, Reason}
    end;
parse(#csv{quote = Quote} = Opts,
      #csv_state{state = fstart, line_in = Line} = State) ->
    case string:substr(Line, 1, length(Quote)) of
	Quote ->
	    ?dbg("Got quote (~p)~n", [State]),
	    parse(Opts, State#csv_state{state = fdata, quote_fl = 1,
					line_in = string:substr(Line,
								length(Quote) + 1)});
	_ ->
	    parse(Opts, State#csv_state{state = fdata})
    end;
parse(Opts, #csv_state{state = fdata, line_in = Line, rec_out = Rec,
		       field = Field} = State) ->
    ?dbg("State: ~p~n", [State]),
    Idx_sep = string:str(Line, Opts#csv.sep),
    Idx_eor = string:str(Line, Opts#csv.eor),
    Idx_quote = string:str(Line, Opts#csv.quote),
    L = lists:filter(fun(X) -> X =/= 0 end, [Idx_sep, Idx_eor, Idx_quote]),
    case L of
	[] ->
	    Field_new = Field ++ Line,
	    case State#csv_state.quote_fl of
		0 ->
		    call_cb(Opts#csv.cb_parse, Opts,
			    lists:reverse([Field_new|Rec]));
		_ ->
		    {error, want_next_line, State#csv_state{line_in = "",
							    field = Field_new}}
	    end;
	_ ->
	    Result = case lists:min(L) of
			 Idx_quote ->
			     ?dbg("Find quote at ~p (~p)~n",
				  [Idx_quote, State]),
			     proc_quote(Opts, State, Idx_quote);
			 Idx_sep ->
			     ?dbg("Find sep at ~p (~p)~n",
				  [Idx_sep, State]),
			     proc_sep(Opts, State, Idx_sep);
			 Idx_eor ->
			     ?dbg("Find eor at ~p (~p)~n",
				  [Idx_eor, State]),
			     proc_eor(Opts, State, Idx_eor)
		     end,
	    case Result of
		{ok, State_new} ->
		    parse(Opts, State_new);
		{error, _, _State_new} ->
		    Result
	    end
    end;
parse(Opts, #csv_state{state = rend, rec_out = Rec} = _State) ->
    ?dbg("State: ~p~n", [State]),
    call_cb(Opts#csv.cb_parse, Opts, lists:reverse(Rec)).


-spec get_rec(csv(), file:io_device()) -> eof |
					  {ok, Rec::[string()]} |
					  {error, Reason::any()}.
%%
%% @doc Get a CSV record from a Fio.
%%
get_rec(Opts, Fio) ->
    case get_rec(Opts, Fio, 1) of
	{error, Reason, _State, _Lnum} ->
	    {error, Reason};
	Ret ->
	    Ret
    end.

get_rec(Opts, Fio, Lnum) ->
    get_rec(Opts, Fio, #csv_state{}, Lnum, {ok, []}).

get_rec(Opts, Fio, State, Lnum, Prev_ret) ->
    case file:read_line(Fio) of
	eof ->
	    case element(1, Prev_ret) of
		error ->
		    Prev_ret;
		_ ->
		    eof
	    end;
	{ok, Line} ->
	    Ret = parse(Opts, State#csv_state{line_in = Line}),
	    case Ret of
		{ok, Rec} ->
		    {ok, Rec, Lnum};
		{error, want_next_line, State_new} ->
		    get_rec(Opts, Fio, State_new, Lnum + 1,
			   {error, want_next_line, State_new, Lnum});
		{error, Reason, State_new} ->
		    {error, Reason, State_new, Lnum}
	    end;
	Ret ->
	    Ret
    end.


-spec get_recs(csv(), file:io_device()) -> eof |
					   {ok, Recs::[[string()]]} |
					   {error, Reason::any()}.
%%
%% @doc Get all CSV records from a Fio.
%%
get_recs(Opts, Fio) ->
    get_recs(Opts, Fio, [], get_rec(Opts, Fio, 1)).

get_recs(Opts, Fio, Recs, {ok, Rec, Lnum}) ->
    get_recs(Opts, Fio, [Rec|Recs], get_rec(Opts, Fio, Lnum + 1));
get_recs(_Opts, _Fio, Recs, eof) ->
    {ok, lists:reverse(Recs)};
get_recs(_Opts, _Fio, _Recs, {error, Reason, _State, Lnum}) ->
    {error, [{error, Reason}, {at_line, Lnum}]}.


-spec get_frecs(csv(), string()) -> eof |
				    {ok, Recs::[[string()]]} |
				    {error, Reason::any()}.
%%
%% @doc Get all CSV records from a file.
%%
get_frecs(Opts, Fname) ->
    case file:open(Fname, [read]) of
	{ok, F} ->
	    Ret = get_recs(Opts, F),
	    file:close(F),
	    Ret;
	Ret ->
	    Ret
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
-spec cb_rec_to_list(tuple()) -> {ok, list()}.
%%
%% @doc Convert a record Rec to a list and remove first element after
%%      it (a record name).
%%
cb_rec_to_list(Rec) ->
    {ok, tl(tuple_to_list(Rec))}.


-spec cb_list_to_rec([string()], atom()) -> {ok, tuple()}.
%%
%% @doc Convert a list of fields to a record with Rec_name name.
%%
cb_list_to_rec(Rec, Rec_name) ->
    {ok, list_to_tuple([Rec_name|Rec])}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec call_cb(undefined | fun(), csv(), any()) -> {ok, Record::any()} |
						  {error, Reason::any()}.
%%
%% @doc Call a Func if it's not undefined.
%%
call_cb(Func, Opts, Rec) ->
    case Func of
	undefined ->
	    {ok, Rec};
	Func ->
	    case Func(Opts, Rec) of
		{ok, Data} ->
		    {ok, Data};
		{error, Reason} ->
		    {error, Reason}
	    end
    end.


-spec mk_field(csv(), field()) -> string().
%%
%% @doc Make an escaped and quoted CSV field from F string.
%%
mk_field(Opts, F) when is_float(F) ->
    mk_field(Opts, float_to_list(F));
mk_field(Opts, F) when is_integer(F) ->
    mk_field(Opts, integer_to_list(F));
mk_field(Opts, F) when is_atom(F) ->
    mk_field(Opts, atom_to_list(F));
mk_field(Opts, F) when is_list(F) ->
    Has_sep = string:str(F, Opts#csv.sep),
    Has_eor = string:str(F, Opts#csv.eor),
    {Has_quote, F_new} = escape_field(F, Opts#csv.quote),
    case (Has_sep bor Has_eor bor Has_quote) of
	0 ->
	    F_new;
	_ ->
	    Opts#csv.quote ++ F_new ++ Opts#csv.quote
    end.


escape_field(F, Quote_str) ->
    escape_field(F, Quote_str, [], 0).

escape_field([], _Quote_str, Acc, Quote_fl) ->
    {Quote_fl, Acc};
escape_field(F, Quote_str, Acc, Quote_fl) ->
    case string:str(F, Quote_str) of
	0 ->
	    {Quote_fl, Acc ++ F};
	Idx ->
	    Len = string:len(Quote_str),
	    New_acc = Acc ++ string:sub_string(F, 1, Idx + Len - 1) ++
		Quote_str,
	    escape_field(string:substr(F, Idx + Len), Quote_str, New_acc, 1)
    end.


proc_quote(_Opts, #csv_state{state = fdata, quote_fl = 0} = State,
	   _Idx_quote) ->
    {error, "Quote in an unquoted field", State};
proc_quote(#csv{quote = Quote, sep = Sep, eor = Eor},
	   #csv_state{state = fdata, quote_fl = 1, line_in = Line} = State,
	   Idx_quote) ->
    Str = string:substr(Line, Idx_quote + length(Quote)),
    case get_prefix([Quote, Sep, Eor], Str) of
	Quote ->
            % The next is the quote
	    S = string:sub_string(Line, 1, Idx_quote + length(Quote) - 1),
	    Field_new = State#csv_state.field ++ S,
	    Line_new = string:substr(Line, Idx_quote + length(Quote)*2),
	    {ok, State#csv_state{line_in = Line_new, field = Field_new}};
	false ->
	    case Str of
		[] ->
		    % The next is the line end
		    S = string:sub_string(Line, 1, Idx_quote - 1),
		    Field_new = State#csv_state.field ++ S,
		    Line_new = Str,
		    {ok, State#csv_state{quote_fl = 0,
					 line_in = Line_new, field = Field_new}};
		_ ->
		    {error, "Quote is unescaped in the field", State}
	    end;
	_ ->
            % The next is the field separator or
            % the next is the record end
	    S = string:sub_string(Line, 1, Idx_quote - 1),
	    Field_new = State#csv_state.field ++ S,
	    Line_new = Str,
	    {ok, State#csv_state{quote_fl = 0,
				 line_in = Line_new, field = Field_new}}
    end.


proc_sep(Opts, #csv_state{quote_fl = 0, line_in = Line} = State, Idx_sep) ->
    S = string:sub_string(Line, 1, Idx_sep - 1),
    Field_new = State#csv_state.field ++ S,
    Rec_new = [Field_new|State#csv_state.rec_out],
    Line_new = string:substr(Line,
			     Idx_sep + length(Opts#csv.sep)),
    {ok, State#csv_state{state = fstart, quote_fl = 0,
			 line_in = Line_new, rec_out = Rec_new,
			 field = []}};
proc_sep(Opts, #csv_state{quote_fl = 1, line_in = Line} = State, Idx_sep) ->
    S = string:sub_string(Line, 1, Idx_sep + length(Opts#csv.sep) - 1),
    Field_new = State#csv_state.field ++ S,
    Line_new = string:substr(Line,
			     Idx_sep + length(Opts#csv.sep)),
    {ok, State#csv_state{line_in = Line_new, field = Field_new}}.


proc_eor(Opts, #csv_state{quote_fl = 0, line_in = Line} = State, Idx_eor) ->
    S = string:sub_string(Line, 1, Idx_eor - 1),
    Field_new = State#csv_state.field ++ S,
    Rec_new = [Field_new|State#csv_state.rec_out],
    Line_new = string:substr(Line,
			     Idx_eor + length(Opts#csv.eor)),
    {ok, State#csv_state{state = rend, quote_fl = 0,
			 line_in = Line_new, rec_out = Rec_new,
			 field = []}};
proc_eor(Opts, #csv_state{quote_fl = 1, line_in = Line} = State, Idx_eor) ->
    S = string:sub_string(Line, 1, Idx_eor + length(Opts#csv.eor) - 1),
    Field_new = State#csv_state.field ++ S,
    Line_new = string:substr(Line,
			     Idx_eor + length(Opts#csv.eor)),
    {error, want_next_line, State#csv_state{line_in = Line_new,
					    field = Field_new}}.


-spec get_prefix([string()], string()) -> string() | false.
%%
%% @doc Find a one of strings from Strs at a start of Str.
%%      Return a matched string from Strs or false otherwise.
%%
get_prefix([], _Str) ->
    false;
get_prefix(_Prefixes, []) ->
    false;
get_prefix([P|Prefixes], Str) ->    
    case lists:prefix(P, Str) of
	true ->
	    P;
	false ->
	    get_prefix(Prefixes, Str)
    end.

%% End of Module.
