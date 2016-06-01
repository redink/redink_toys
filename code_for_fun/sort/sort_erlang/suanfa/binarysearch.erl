%% don't use ets:slot/2

-module(binarysearch).
-export([start/1, init_table/1]).

init_table(TableSize) ->
    ets:new(t, [named_table, ordered_set]),
    [ets:insert(t, {X, value}) || X <- [random:uniform(1000000000) || _ <- lists:seq( 1, TableSize)]],
    t.

start(Value) ->
    

    ets:new(t1, [named_table, public]),
    ets:insert(t1, {k, 0}),
    

    Return =
        case ets:lookup(t, Value) of
            [{Value, _}] ->
                Value;
            [] ->
                case (R = ets:first(t)) > Value of
                    true ->
                        R;
                    false ->
                        case (R1 = ets:last(t)) < Value of
                            true ->
                                R1;
                            _ ->
                                start(1, ets:info(t, size), Value, t, 1)
                        end
                end
        end,
    RRR = ets:tab2list(t1),
    ets:delete(t1),
    {RRR, Return}.

start(Start, End, Value, Table, Num) ->
    case Num < ets:info(Table, size) of
        true ->
            ets:update_counter(t1, k, 1),
            M = (Start + End) div 2,
            [{MK, _}] = ets:slot(Table, M),
            case MK > Value of
                true ->
                    case (R = ets:prev(Table, MK)) < Value of
                        true ->
                            {MK, R};
                        false ->
                            start(Start, M, Value, Table, Num + 1)
                    end;
                false ->
                    start(M, End, Value, Table, Num + 1)
            end;
        _ ->
            failed
    end.

% ****** Process <0.73.0>    -- 100.00 % of profiled time *** 
% FUNCTION                       CALLS        %  TIME  [uS / CALLS]
% --------                       -----  -------  ----  [----------]
% orddict:new/0                      1     0.00     0  [      0.00]
% erl_eval:eval_fun/2                1     0.00     0  [      0.00]
% erl_eval:guard/4                   1     0.00     0  [      0.00]
% erl_eval:merge_bindings/2          2     0.00     0  [      0.00]
% erl_internal:bif/3                 1     0.00     0  [      0.00]
% erl_eval:exprs/5                   1     0.02     1  [      1.00]
% erl_eval:eval_fun/6                1     0.02     1  [      1.00]
% erl_eval:expr_list/6               2     0.02     1  [      0.50]
% erl_eval:guard0/4                  1     0.02     1  [      1.00]
% erl_eval:match_list/4              1     0.02     1  [      1.00]
% erl_eval:new_bindings/0            1     0.02     1  [      1.00]
% erl_eval:add_bindings/2            1     0.02     1  [      1.00]
% erl_eval:'-expr/5-fun-3-'/1        1     0.02     1  [      1.00]
% ets:tab2list/1                     1     0.02     1  [      1.00]
% lists:reverse/1                    1     0.02     1  [      1.00]
% lists:foldl/3                      3     0.02     1  [      0.33]
% binarysearch:start/1               1     0.02     1  [      1.00]
% shell:apply_fun/3                  1     0.02     1  [      1.00]
% shell:'-eval_loop/3-fun-0-'/3      1     0.02     1  [      1.00]
% ets:first/1                        1     0.02     1  [      1.00]
% orddict:to_list/1                  3     0.03     2  [      0.67]
% erl_eval:expr/5                    4     0.03     2  [      0.50]
% erl_eval:do_apply/6                1     0.03     2  [      2.00]
% erl_eval:ret_expr/3                3     0.03     2  [      0.67]
% erl_eval:expr_list/4               1     0.03     2  [      2.00]
% ets:insert/2                       1     0.03     2  [      2.00]
% erlang:apply/2                     1     0.05     3  [      3.00]
% ets:last/1                         1     0.05     3  [      3.00]
% ets:delete/1                       1     0.05     3  [      3.00]
% ets:lookup/2                       1     0.08     5  [      5.00]
% ets:new/2                          1     0.09     6  [      6.00]
% ets:match_object/2                 1     0.11     7  [      7.00]
% binarysearch:start/5              17     0.12     8  [      0.47]
% ets:info/2                        18     0.17    11  [      0.61]
% ets:update_counter/3              17     0.21    14  [      0.82]
% ets:prev/2                        15     0.38    25  [      1.67]
% ets:slot/2                        17    98.29  6428  [    378.12]
% -----------------------------  -----  -------  ----  [----------]
% Total:                           127  100.00%  6540  [     51.50]
% ok
