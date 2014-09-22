#!/usr/bin/env escript
%%! -name redink_binary_test@127.0.0.1


-define(debugMsg(S),
    begin
        io:fwrite(user, <<"~s:~w:~w: ~s\n">>,
              [?FILE, ?LINE, self(), S]),
        ok
    end).
-define(debugHere, (?debugMsg("<-"))).
-define(debugFmt(S, As), (?debugMsg(io_lib:format((S), (As))))).
-define(debugVal(E),
    begin
    ((fun (__V) ->
          ?debugFmt(<<"~s = ~P">>, [(??E), __V, 100]),
          __V
      end)(E))
    end).


main(_) ->

    FlagList  = [timestamp,  set_on_spawn, garbage_collection],

    Tracer = spawn_link(fun() ->
                        lists:foreach(
                        fun(_) ->
                            receive
                                TracerMsg ->
                                    ?debugVal(TracerMsg)
                            end
                        end, lists:seq(1, 1000000))
                        
                        end),

    erlang:trace(self(), true, [{tracer,  Tracer} | FlagList]),

    %% some code operation 

    Ref = erlang:trace_delivered(all),                                               
    receive                                                                          
        {trace_delivered, all, Ref} -> ok                                            
    end,    

    ok.
