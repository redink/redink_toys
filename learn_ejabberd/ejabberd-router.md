# ejabberd router
```
(ejabberd@localhost)9> ets:tab2list(route).                                 
[#route{domain = <<"irc.localhost">>,
        server_host = <<"localhost">>,pid = <0.591.0>,
        local_hint = undefined},
 #route{domain = <<"conference.localhost">>,
        server_host = <<"localhost">>,pid = <0.377.0>,
        local_hint = undefined},
 #route{domain = <<"localhost">>,
        server_host = <<"localhost">>,pid = <0.303.0>,
        local_hint = {apply,ejabberd_local,route}},
 #route{domain = <<"pubsub.localhost">>,
        server_host = <<"localhost">>,pid = <0.546.0>,
        local_hint = undefined}]
(ejabberd@localhost)10> erlang:process_info(erlang:list_to_pid("<0.591.0>"), [registered_name, dictionary]).
[{registered_name,ejabberd_mod_irc_localhost},
 {dictionary,[{'$initial_call',{mod_irc,init,1}},
              {'$ancestors',[ejabberd_sup,<0.66.0>]}]}]
(ejabberd@localhost)11> erlang:process_info(erlang:list_to_pid("<0.377.0>"), [registered_name, dictionary]).
[{registered_name,ejabberd_mod_muc_localhost},
 {dictionary,[{'$initial_call',{mod_muc,init,1}},
              {'$ancestors',[ejabberd_sup,<0.66.0>]}]}]
(ejabberd@localhost)12> erlang:process_info(erlang:list_to_pid("<0.303.0>"), [registered_name, dictionary]).
[{registered_name,ejabberd_local},
 {dictionary,[{'$initial_call',{ejabberd_local,init,1}},
              {'$ancestors',[ejabberd_sup,<0.66.0>]}]}]
(ejabberd@localhost)13> erlang:process_info(erlang:list_to_pid("<0.546.0>"), [registered_name, dictionary]).
[{registered_name,ejabberd_mod_pubsub_localhost},
 {dictionary,[{'$initial_call',{mod_pubsub,init,1}},
              {'$ancestors',[ejabberd_sup,<0.66.0>]}]}]
```
## `ejabberd_router` module
`ejabberd_router` 是 ejabberd 中所有stanza 数据包的总线。其他模块通过register 的方式作为二级router 进行下一层级的数据包分发。理论上，二级router是可以无穷尽扩展的，默认配置下，二级router 有：
- 1 local
  - module: ejabberd_local
  - domain: localhost
  - 常规数据包的route
  - route 方式: message send/MFA call
- 2 pubsub
  - module: mod_pubsub
  - domain: pubsub.localhost
  - 负责pubsub 数据包的route
  - route 方式: message send
- 3 muc
  - module: mod_muc
  - domain: conference.localhost
  - 负责 muc 数据包的route
  - route 方式: message send
- 4 irc
  - module: mod_irc
  - domain: irc.localhost
  - 负责 irc 数据包的route
  - route 方式: message send

数据包在二级router 中，会调用 ejabberd_sm 的相关函数，找到TO 的c2s 进程，最终将数据发送到对应的客户端。
## 二级 route 存取方式
二级router 向`ejabberd_router` 注册，是调用`ejabberd_router:register_route/3`函数，将route 信息写入到mnesia 数据表中。而当stanza 数据包到达 `ejabberd_router` 时，`ejabberd_router` module 会根据 domain 查找相对应的route，通过 message send （或 MFA call）的方式将数据包route 到二级router。
## local router
在二级router local中，route 的主要流程是：

- luser /= <<"">> ===> ejabberd_sm:route/3
- luser == <<"">> andalso lresource == <<"">> ===>
  - packetname == iq       ---> process_iq
  - packetname == message  ---> ok/error
  - packetname == presence ---> ok
  - packetname == `_other`   ---> ok
- luser == <<"">> andaslo lresouce /= <<"">> ===>
  - packettype == error ---> ok
  - packettype == result ---> ok
  - packettype == `_other` ---> run hooks for local_send_to_resource_hook

```
do_route(From, To, Packet) ->
    if
        To#jid.luser /= <<"">> ->
            ejabberd_sm:route(From, To, Packet);
        To#jid.lresource == <<"">> ->
            #xmlel{name = Name} = Packet,
            case Name of
                <<"iq">> ->
                    %% （后面再说）
                    process_iq(From, To, Packet);
                <<"message">> ->
                    #xmlel{attrs = Attrs} = Packet,
                    case fxml:get_attr_s(<<"type">>, Attrs) of
                        <<"headline">> -> ok;
                        <<"error">> -> ok;
                        _ ->
                            Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
                            ejabberd_router:route(To, From, Err)
                    end;
                <<"presence">> -> ok;
                _ -> ok
            end;
        true ->
            #xmlel{attrs = Attrs} = Packet,
            case fxml:get_attr_s(<<"type">>, Attrs) of
                <<"error">> -> ok;
                <<"result">> -> ok;
                _ ->
                    ejabberd_hooks:run(local_send_to_resource_hook, To#jid.lserver, [From, To, Packet])
            end
    end.
```
## 其他 router
其他router 和local router 的原理基本相同，都是在handle_info callback 下接收`ejabberd_router`发送过来的 route 消息。

而不同于local 的地方在于，不同的逻辑处理流程，以及最终获取的session和最后发送到的客户端。