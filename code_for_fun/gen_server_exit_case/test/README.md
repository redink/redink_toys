# gen server exit case 

## test flow
```
$ ./rebar com 
==> test (compile)
Compiled src/test_sup.erl
Compiled src/test_app.erl
src/test_svr.erl:64: Warning: no clause will ever match
src/test_svr.erl:72: Warning: no clause will ever match
src/test_svr.erl:81: Warning: no clause will ever match
src/test_svr.erl:90: Warning: no clause will ever match
src/test_svr.erl:110: Warning: no clause will ever match
src/test_svr.erl:117: Warning: no clause will ever match
src/test_svr.erl:130: Warning: no clause will ever match
Compiled src/test_svr.erl

%% ignore warning, just for test

$ erl -pa ./ebin 
Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.2.1  (abort with ^G)
1> application:start(test).
ok
2> erlang:whereis(test_svr).
<0.39.0>
%% first test case 
3> erlang:self().
<0.33.0>
4> gen_server:call(test_svr, test_caller_timeout).
** exception exit: {timeout,{gen_server,call,[test_svr,test_caller_timeout]}}
     in function  gen_server:call/2 (gen_server.erl, line 204)
5> erlang:self().
<0.43.0>
6> erlang:whereis(test_svr).
<0.39.0>
%% `test_svr` process is normal, caller is exit

7> gen_server:call(test_svr, test_spawn_link_process_exit).
ok
8> 
=ERROR REPORT==== 7-Apr-2016::18:26:34 ===
Error in process <0.47.0> with exit value:
{{badmatch,2},
 [{test_svr,'-handle_call/3-fun-0-',0,[{file,"src/test_svr.erl"},{line,64}]}]}

8> erlang:whereis(test_svr).
<0.48.0>
%% `test_svr` process exited, (but supervisor restart it)

9> gen_server:call(test_svr, test_spawn_process_exit).

=ERROR REPORT==== 7-Apr-2016::18:28:02 ===
Error in process <0.51.0> with exit value:
{{badmatch,2},
 [{test_svr,'-handle_call/3-fun-1-',0,[{file,"src/test_svr.erl"},{line,72}]}]}
ok
10> erlang:whereis(test_svr).                          
<0.48.0>
%% `test_svr` process is normal

11> gen_server:call(test_svr, {trap_exit, test_spawn_link_process_exit}).

=ERROR REPORT==== 7-Apr-2016::18:28:44 ===
Error in process <0.54.0> with exit value:
{{badmatch,2},
 [{test_svr,'-handle_call/3-fun-2-',0,[{file,"src/test_svr.erl"},{line,81}]}]}
ok
12> erlang:whereis(test_svr).                                            
<0.48.0>
%% `test_svr` process is normal too.

13> gen_server:call(test_svr, {trap_exit, test_spawn_process_exit}).

=ERROR REPORT==== 7-Apr-2016::18:29:18 ===
Error in process <0.57.0> with exit value:
{{badmatch,2},
 [{test_svr,'-handle_call/3-fun-3-',0,[{file,"src/test_svr.erl"},{line,90}]}]}
ok
14> erlang:whereis(test_svr).                                       
<0.48.0>
%% `test_svr` process is normal too.

15> gen_server:call(test_svr, test_spawn_genserver_process_exit).
ok

=ERROR REPORT==== 7-Apr-2016::18:38:37 ===
** Generic server sub_process terminating 
** Last message in was send_self_msg
** When Server state == {state}
** Reason for termination == 
** {{badmatch,2},
    [{test_svr,handle_info,2,[{file,"src/test_svr.erl"},{line,132}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,615}]},
     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,681}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,240}]}]}
16> erlang:whereis(test_svr).
<0.62.0>

%% `test_svr` process exited too.
17> gen_server:call(test_svr, {trap_exit, test_spawn_genserver_process_exit}).

=ERROR REPORT==== 7-Apr-2016::18:39:14 ===
** Generic server sub_process terminating 
** Last message in was send_self_msg
** When Server state == {state}
** Reason for termination == 
** {{badmatch,2},
    [{test_svr,handle_info,2,[{file,"src/test_svr.erl"},{line,132}]},
     {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,615}]},
     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,681}]},
     {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,240}]}]}
ok
18> erlang:whereis(test_svr).                                                 
<0.62.0>

%% `test_svr` process is normal
19> gen_server:call(test_svr, test_spawn_genserver_process_and_parent_exit).
 sub process is : {ok,<0.67.0>}

=ERROR REPORT==== 7-Apr-2016::18:41:07 ===
** Generic server <0.62.0> terminating 
** Last message in was test_spawn_genserver_process_and_parent_exit
** When Server state == {state}
** Reason for termination == 
** {{badmatch,2},
    [{test_svr,handle_call,3,[{file,"src/test_svr.erl"},{line,112}]},
     {gen_server,try_handle_call,4,[{file,"gen_server.erl"},{line,629}]},
     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,661}]},
     {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,250}]}]}
** exception exit: {{{badmatch,2},
                     [{test_svr,handle_call,3,
                                [{file,"src/test_svr.erl"},{line,112}]},
                      {gen_server,try_handle_call,4,
                                  [{file,"gen_server.erl"},{line,629}]},
                      {gen_server,handle_msg,5,
                                  [{file,"gen_server.erl"},{line,661}]},
                      {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,250}]}]},
                    {gen_server,call,
                                [test_svr,test_spawn_genserver_process_and_parent_exit]}}
     in function  gen_server:call/2 (gen_server.erl, line 204)
20> erlang:whereis(sub_process).
undefined

%% the `sub_process` process exited too.
21> gen_server:call(test_svr, {trap_exit, test_spawn_genserver_process_and_parent_exit}).
 sub process is : {ok,<0.72.0>}

=ERROR REPORT==== 7-Apr-2016::18:41:54 ===
** Generic server <0.68.0> terminating 
** Last message in was {trap_exit,test_spawn_genserver_process_and_parent_exit}
** When Server state == {state}
** Reason for termination == 
** {{badmatch,2},
    [{test_svr,handle_call,3,[{file,"src/test_svr.erl"},{line,119}]},
     {gen_server,try_handle_call,4,[{file,"gen_server.erl"},{line,629}]},
     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,661}]},
     {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,250}]}]}

=ERROR REPORT==== 7-Apr-2016::18:41:54 ===
** Generic server sub_process terminating 
** Last message in was {'EXIT',<0.68.0>,
                           {{badmatch,2},
                            [{test_svr,handle_call,3,
                                 [{file,"src/test_svr.erl"},{line,119}]},
                             {gen_server,try_handle_call,4,
                                 [{file,"gen_server.erl"},{line,629}]},
                             {gen_server,handle_msg,5,
                                 [{file,"gen_server.erl"},{line,661}]},
                             {proc_lib,wake_up,3,
                                 [{file,"proc_lib.erl"},{line,250}]}]}}
** When Server state == {state}
** Reason for termination == 
** {{badmatch,2},
    [{test_svr,handle_call,3,[{file,"src/test_svr.erl"},{line,119}]},
     {gen_server,try_handle_call,4,[{file,"gen_server.erl"},{line,629}]},
     {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,661}]},
     {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,250}]}]}
** exception exit: {{{badmatch,2},
                     [{test_svr,handle_call,3,
                                [{file,"src/test_svr.erl"},{line,119}]},
                      {gen_server,try_handle_call,4,
                                  [{file,"gen_server.erl"},{line,629}]},
                      {gen_server,handle_msg,5,
                                  [{file,"gen_server.erl"},{line,661}]},
                      {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,250}]}]},
                    {gen_server,call,
                                [test_svr,
                                 {trap_exit,test_spawn_genserver_process_and_parent_exit}]}}
     in function  gen_server:call/2 (gen_server.erl, line 204)
22> erlang:whereis(sub_process).                                                         
undefined

%% the `sub_process` process exited too. 
```