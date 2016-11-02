# ejabberd hooks
在ejabberd 中，hooks 是非常重要的一个角色。是保证ejabberd 高可扩展性的一个重要依赖。通过 add hooks 的方式，可以为某个 event register 相应的action，并在event 发生时，触发所有相对应的actions.
## add hook
**add hook 是不同的module 通过调用 `ejabberd_hooks:add/N`实现hook 的add 逻辑。**

add hook 实现的具体流程：
- 1 action module 调用`ejabberd_hooks:add/N`
- 2 gen_server call ejabberd_hooks 进程
- 3 ejabberd_hooks 进程在 handle_call callback 中实现 add

handle_call callback 的函数实现：
```
handle_call({add, Hook, Host, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Module, Function},
    Reply = handle_add(Hook, Host, HookFormat),
    {reply, Reply, State};
handle_call({add, Hook, Host, Node, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Node, Module, Function},
    Reply = handle_add(Hook, Host, HookFormat),
    {reply, Reply, State};
```

具体的 add 逻辑是将新的 {{hook, host}, {seq, module, function}} 写入到ets table 中，如果在ets table 中，已经有{hook, host} 对应的hook 存在，就进行merge。最终，在ets table 中，单条记录类似于：
```
{{remove_user,<<"localhost">>},
  [{50,mod_last,remove_user},
   {50,mod_offline,remove_user},
   {50,mod_privacy,remove_user},
   {50,mod_private,remove_user},
   {50,mod_pubsub,remove_user},
   {50,mod_roster,remove_user},
   {50,mod_shared_roster,remove_user},
   {50,mod_vcard,remove_user},
   {100,ejabberd_sm,disconnect_removed_user}]}
```
而实现代码：
```
-spec handle_add(atom(), atom(), local_hook() | distributed_hook()) -> ok.
handle_add(Hook, Host, El) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            case lists:member(El, Ls) of
                true ->
                    %% 已经存在
                    ok;
                false ->
                    %% 不存在，进行merge，这也是为什么add 逻辑得在 handle_call 实现的原因
                    NewLs = lists:merge(Ls, [El]),
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    ok
            end;
        [] ->
            NewLs = [El],
            ets:insert(hooks, {{Hook, Host}, NewLs}),
            ok
    end.
```
## run hook
run hook 使用类似于 『event』 的原理，在发生某个event 的时候，run 相对应的 hook，从而执行该hook 对应的所有 actions。
`ejabberd:run/2,3`的定义：
```
run(Hook, Args) ->
    run(Hook, global, Args).
run(Hook, Host, Args) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            run1(Ls, Hook, Args);
        [] ->
            ok
    end.
```
通过`ets:lookup/2` 获取到 actions 列表之后，调用 run1/3 遍历列表，依次执行相对应的function:
```
run1([], _Hook, _Args) -> ok;
run1([{_Seq, Node, Module, Function} | Ls], Hook, Args) ->
    case ejabberd_cluster:call(Node, Module, Function, Args) of
        timeout          -> run1(Ls, Hook, Args);
        {badrpc, Reason} -> run1(Ls, Hook, Args);
        stop             -> ok;
        Res              -> run1(Ls, Hook, Args)
    end;
run1([{_Seq, Module, Function} | Ls], Hook, Args) ->
    case safe_apply(Module, Function, Args) of
        {'EXIT', Reason} -> run1(Ls, Hook, Args);
        stop             -> ok;
        _                -> run1(Ls, Hook, Args)
    end.
```
run_fold 的流程和 run 的流程基本上一样，主要的区别在于run_fold 需要记录中间结果（中间结果是下一个action 的输入参数），并返回最终结果。
## 主要的 hook event actions 规理

| event | hook | actions |
|:----- |:----:| -------:|
| `reopen_log` | `reopen_log_hook` | `mod_http_fileserver:reopen_log/1`|
| `try_register` <br> `register_connection` | `register_user` | `mod_last:register_user/2` <br> `mod_shared_roster:register_user/2` |
| `remove_user` | `remove_user` | `ejabberd_sm:disconnect_removed_user` <br> `mod_http_unload:remove_user/2` <br> `mod_last:remove_user/2` <br> `mod_mam:remove_user/2` <br> `mod_offline:remove_user/2` <br> `mod_privacy:remove_user/2` <br> `mod_private:remove_user/2` <br> `mod_pubsub:remove_user/2` <br> `mod_roster:remove_user/2` <br> `mod_shared_roster:remove_user/2` <br> `mod_vcard:remove_user/2` |
| `auth in c2s` | `c2s_auth_result` | `mod_fail2ban:c2s_auth_result/4` |
| `presence_update` | `user_available_hook` | `mod_announce:send_motd/1` <br> `mod_shared_roster:user_available/1` |
| | `roster_out_subscription` | |
| | `local_send_to_resource_hook` | |
| | `sm_register_connection_hook` | |
| | `sm_remove_connection_hook` | |
| | `set_presence_hook` | |
| | `unset_presence_hook` | |
| `ejabberd_sm route_mesage` | `offline_message_hook` | `mod_metrics:offline_message_hook/3` <br> `mod_offline:store_packet/3` <br> `ejabberd_sm:bounce_offline_message/3` |
| | `remove_room` | |
| | `muc_invite` | |
| | `pubsub_create_node` | |
| | `pubsub_delete_node` | |
| | `pubsub_subscribe_node` | |
| | `pubsub_unsubscribe_node` | |
| | `pubsub_publish_item` | |
