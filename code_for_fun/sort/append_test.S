{module, append_test}.  %% version = 0

{exports, [{module_info,0},
           {module_info,1},
           {'test++',0},
           {'test++',1},
           {'test++',2},
           {'test++++',2},
           {'test|',0},
           {'test|',1},
           {'test|',2}]}.

{attributes, []}.

{labels, 19}.


{function, 'test++', 0, 2}.
  {label,1}.
    {line,[{location,"append_test.erl",5}]}.
    {func_info,{atom,append_test},{atom,'test++'},0}.
  {label,2}.
    {move,{literal,[1,2,3]},{x,0}}.
    return.


{function, 'test++', 1, 4}.
  {label,3}.
    {line,[{location,"append_test.erl",7}]}.
    {func_info,{atom,append_test},{atom,'test++'},1}.
  {label,4}.
    {test_heap,2,1}.
    {put_list,{x,0},{literal,[2,3]},{x,0}}.
    return.


{function, 'test++', 2, 6}.
  {label,5}.
    {line,[{location,"append_test.erl",9}]}.
    {func_info,{atom,append_test},{atom,'test++'},2}.
  {label,6}.
    {test_heap,2,2}.
    {put_list,{x,0},{x,1},{x,0}}.
    return.


{function, 'test|', 0, 8}.
  {label,7}.
    {line,[{location,"append_test.erl",12}]}.
    {func_info,{atom,append_test},{atom,'test|'},0}.
  {label,8}.
    {move,{literal,[1,2,3]},{x,0}}.
    return.


{function, 'test|', 1, 10}.
  {label,9}.
    {line,[{location,"append_test.erl",14}]}.
    {func_info,{atom,append_test},{atom,'test|'},1}.
  {label,10}.
    {test_heap,2,1}.
    {put_list,{x,0},{literal,[2,3]},{x,0}}.
    return.


{function, 'test|', 2, 12}.
  {label,11}.
    {line,[{location,"append_test.erl",16}]}.
    {func_info,{atom,append_test},{atom,'test|'},2}.
  {label,12}.
    {test_heap,2,2}.
    {put_list,{x,0},{x,1},{x,0}}.
    return.


{function, 'test++++', 2, 14}.
  {label,13}.
    {line,[{location,"append_test.erl",19}]}.
    {func_info,{atom,append_test},{atom,'test++++'},2}.
  {label,14}.
    {line,[{location,"append_test.erl",20}]}.
    {call_ext_only,2,{extfunc,erlang,'++',2}}.


{function, module_info, 0, 16}.
  {label,15}.
    {line,[]}.
    {func_info,{atom,append_test},{atom,module_info},0}.
  {label,16}.
    {move,{atom,append_test},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 18}.
  {label,17}.
    {line,[]}.
    {func_info,{atom,append_test},{atom,module_info},1}.
  {label,18}.
    {move,{x,0},{x,1}}.
    {move,{atom,append_test},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
