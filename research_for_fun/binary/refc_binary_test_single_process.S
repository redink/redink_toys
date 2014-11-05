{module, refc_binary_test_single_process}.  %% version = 0

{exports, [{handle_big_binary,0},{module_info,0},{module_info,1},{start,0}]}.

{attributes, []}.

{labels, 10}.


{function, start, 0, 2}.
  {label,1}.
    {line,[{location,"refc_binary_test_single_process.erl",6}]}.
    {func_info,{atom,refc_binary_test_single_process},{atom,start},0}.
  {label,2}.
    {allocate,0,0}.
    {line,[{location,"refc_binary_test_single_process.erl",7}]}.
    {call,0,{f,4}}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,binary_info}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",8}]}.
    {call_ext,1,{extfunc,erts_debug,get_internal_state,1}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,"------- handled ~p~n"},{x,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",8}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {move,{integer,1000000},{x,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",9}]}.
    {call_ext_last,1,{extfunc,timer,sleep,1},0}.


{function, handle_big_binary, 0, 4}.
  {label,3}.
    {line,[{location,"refc_binary_test_single_process.erl",11}]}.
    {func_info,{atom,refc_binary_test_single_process},
               {atom,handle_big_binary},
               0}.
  {label,4}.
    {allocate_zero,1,0}.
    {move,{integer,1048576},{x,1}}.
    {move,{literal,<<1>>},{x,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",12}]}.
    {call_ext,2,{extfunc,binary,copy,2}}.
    {test_heap,3,1}.
    {move,{x,0},{y,0}}.
    {put_tuple,2,{x,0}}.
    {put,{atom,binary_info}}.
    {put,{y,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",13}]}.
    {call_ext,1,{extfunc,erts_debug,get_internal_state,1}}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,"-------- resource ~p~n"},{x,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",13}]}.
    {call_ext,2,{extfunc,io,format,2}}.
    {test,bs_start_match2,{f,5},0,[{y,0},0],{x,0}}.
    {test,bs_get_binary2,
          {f,5},
          1,
          [{x,0},
           {integer,1},
           8,
           {field_flags,[{anno,[14,
                                {file,"refc_binary_test_single_process.erl"}]},
                         unsigned,big]}],
          {x,1}}.
    {test,bs_test_unit,{f,5},[{x,0},8]}.
    {move,{x,1},{x,0}}.
    {deallocate,1}.
    return.
  {label,5}.
    {bs_context_to_binary,{y,0}}.
    {line,[{location,"refc_binary_test_single_process.erl",14}]}.
    {badmatch,{y,0}}.


{function, module_info, 0, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,refc_binary_test_single_process},{atom,module_info},0}.
  {label,7}.
    {move,{atom,refc_binary_test_single_process},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,refc_binary_test_single_process},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,refc_binary_test_single_process},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
