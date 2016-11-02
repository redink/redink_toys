# stanza route flow
## stanza 分类
根据XMPP协议，消息大概分为：
- presence 主要包括各类状态信息
- message 实际发送的消息
- iq

此部分，详见XMPP协议

## 概览
在ejabberd 中，数据包的首先经由socket module 流入，然后转交到 c2s module，随c2s 进程的状态转换进行数据包流转。首先，c2s 进程的状态主要包括：
- 1 wait_for_stream
- 2 wait_for_feature_request
- 3 wait_for_bind
- 4 session_established
- 5 wait_for_auth
- 6 wait_for_sasl_response
- 7 wait_for_resume

其中，`wait_for_stream` 是c2s 进程的起始状态，`wait_for_bind` 是处理resource 的绑定，`wait_for_auth` 是处理 user/password 的身份校验，而经过XMPP完成各种身份、ssl校验之后，c2s 进程的状态就会进入`session_established` 状态，并长时间停留于此。

* `wait_for_stream` 状态概览

```
wait_for_stream
    streamversion =/ 1.0 ->  wait_for_auth
    streamversion == 1.0 ->
        authenticated == false -> wait_for_feature_request
        authenticated == true  ->
            resource == "" -> wait_for_bind
            resource       -> session_established
```
若`streamversion` 为1.0，就需要身份校验。如果已经通过身份校验并且resource 不为空，c2s 进程状态就转换为`session_established`；而通过身份校验，但是resource 为空，就需要进行bind，c2s 进程状态转变为`wait_for_bind`。

当`streamversion` 不为1.0，c2s 进程状态就立即转变为`wait_for_auth`。

* `wait_for_auth`/`wait_for_bind` 状态概览

`wait_for_auth` 的主要工作是完成 user/password 的身份校验，`wait_for_bind` 是完成user 与 resource 的bind。两个状态成功处理之后，都是将c2s 进程转换为`session_established`状态。
```
wait_for_auth
    auth user/password
        open_session -> session_established
wait_for_bind
    session_established
```
* `wait_for_feature_request`/`wait_for_sasl_response` 状态概述

这两个状态，主要是为了对用户进行sasl 校验，如果校验成功，就将c2s 进程状态转换为`wait_for_stream`（也就是c2s 进程的起始状态），并将`authenticated` 标记为`true`。
```
wait_for_feature_request
    cyrsasl:server_start/3 ->
        1, ok => {wait_for_stream, authenticated = true}
        2, continue => {wait_for_sasl_response}
        3, error => {wait_for_feature_request, send_element(failure)}
wait_for_sasl_response
    cyrsasl:server_step/2 ->
        1, ok => {wait_for_stream, authenticated = true}
        2, continue => {wait_for_sasl_response}
        3, error => {wait_for_feature_request, send_element(failure)}
```
* `session_established` 状态

当完成XMPP 协商以及校验之后，c2s 进程的状态机就会转变为`session_established`，此状态即为数据包流转的主要状态。
```
session_established
    main process loop
```

概览图：
（此处需要补个图）

## stanza 流转主流程
stanza 数据包流转，主要是在 c2s 进程的`session_established` 进行的。

c2s 进程会根据stanza 数据包类型的不同，做相应的处理：

* presence

对于presence 类型的数据包，c2s 进程主要是进行presence update
```
case ToJID of
    #jid{user = User, server = Server, resource = <<"">>} ->
        presence_update(FromJID, PresenceEl, NewStateData);
    _ ->
        presence_track(FromJID, ToJID, PresenceEl, NewStateData)
end;
```

* iq

对于iq 类型的数据包，c2s 进程主要是进行 info/query 的处理
```
case jlib:iq_query_info(NewEl) of
    #iq{xmlns = Xmlns} = IQ when Xmlns == (?NS_PRIVACY); Xmlns == (?NS_BLOCKING) ->
        process_privacy_iq(FromJID, ToJID, IQ, NewStateData);
    #iq{xmlns = ?NS_SESSION} ->
        Res = jlib:make_result_iq_reply(NewEl#xmlel{children = []}),
        send_stanza(NewStateData, Res);
    _ ->
        NewEl0 = ejabberd_hooks:run_fold(user_send_packet, Server, NewEl, [NewStateData, FromJID, ToJID]),
        check_privacy_route(FromJID, NewStateData, FromJID, ToJID, NewEl0)
end;
```

* message

message 是XMPP stanza 数据包的主要数据类型。
```
NewEl0 = ejabberd_hooks:run_fold(user_send_packet, Server, NewEl, [NewStateData, FromJID, ToJID]),
check_privacy_route(FromJID, NewStateData, FromJID, ToJID, NewEl0);
```
c2s 进程会调用 `check_privacy_route/5` 函数。

### `check_privacy_route/5` 函数

`check_privacy_route/5` 函数的实现如下：
```
check_privacy_route(From, StateData, FromRoute, To, Packet) ->
    case privacy_check_packet(StateData, From, To, Packet, out) of
        deny ->
            Lang = StateData#state.lang,
            ErrText = <<"Your active privacy list has denied the routing of this stanza.">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
            Err2 = jlib:replace_from_to(To, From, Err),
            send_stanza(StateData, Err2);
        allow ->
            ejabberd_router:route(FromRoute, To, Packet),
            StateData
    end.
```
首先，该函数会进行`privacy rules` check，如果check 结果为 deny，会错误返回，否则，进入下一层 route，也就是调用 `ejabberd_router:route/3` 函数。

### `ejabberd_router:route/3` 函数
`ejabberd_router:route/3` 会调用做实际的数据包分发：
```
%% ejabberd_route
route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} -> error; %% omit log
        _ -> ok
    end.
do_route(From, To, Packet) ->
    LDstDomain = To#jid.lserver,
    %% 获取 route 规则
    case mnesia:dirty_read(route, LDstDomain) of
        [] ->
            %% 若为空
            ejabberd_s2s:route(From, To, Packet);
        [R] ->
            %% 当且仅当只有一个route
            %% R 对应的pid，属于当前节点，且local_hint 为MFA，就调用MFA(本用户进程调用)
            %% R 对应的pid，不属于当前节点，就将 {route, From, To, Packet} 发送给pid
            %% 除此之外，就将packet drop
        Rs ->
            %% 当前route 中存在多个handler
            Value = xxx, %% 获取 hash term
            case get_component_number(LDstDomain) of
                undefined ->
                    case [R || R <- Rs, node(R#route.pid) == node()] of
                        [] ->
                            %% 所有的pid 都不在当前节点
                            %% 根据hash Value 拿到其中一个pid，然后 erlang:send/2
                        LRs ->
                            %% 有pid 在当前节点
                    end;
                _ ->
                    SRs = lists:ukeysort(#route.local_hint, Rs),
                    R = lists:nth(erlang:phash(Value, length(SRs)), SRs),
                    Pid = R#route.pid,
                    if
                        is_pid(Pid) ->
                            Pid ! {route, From, To, Packet};
                        true -> drop
                    end
            end
    end.
```
### `ejabberd_local:route/3` 函数
在 `ejabberd_local` module 中，有两个入口，一个是函数直接调用，另一个是通过消息发送 handle_info 回调：
```
route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} -> error; %% omit log
        _ -> ok
    end.
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} -> error; %% omit log
        _ -> ok
    end,
    {noreply, State};
```
这两个函数都会调用 `do_route/3` 函数
```
do_route(From, To, Packet) ->
    if
        To#jid.luser /= <<"">> ->
            %% to user 字段不为空， 调用 `ejabberd_sm:route/3`
            ejabberd_sm:route(From, To, Packet);
        ...
    end.
```
此函数会检查 To 的luser 字段，如果不为空，则调用 `ejabberd_sm:route/3` 函数。

此处需要注意的是，如果是local_hit 为 MFA，则通过用户进程调用此函数，不会对 `ejabberd_local` 进程造成负担，但如果是通过"用户进程发送消息"的方式，就有可能给`ejabberd_local` 进程带来负担。

### `ejabberd_sm:route/3`

`ejabberd_sm:route/3` 函数，同样只是一个API接口，随即会调用 do_route/3 函数，do_route/3 函数会根据 packet是否为`broadcast` 做不同的处理。

```
route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
        {'EXIT', Reason} -> error; %% omit log
        _ -> ok
    end.
```

`do_route/3` 函数实现：
```
do_route(From, To, {broadcast, _} = Packet) ->
    case To#jid.lresource of
        <<"">> ->
            ...
        _ ->
            {U, S, R} = jid:tolower(To),
            Mod = get_sm_backend(S),
            %% get session，并将数据packet 发送给 c2s 进程
    end;
do_route(From, To, #xmlel{} = Packet) ->
    #jid{user = User, server = Server, luser = LUser,
         lserver = LServer, lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = Packet,
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    case LResource of
        <<"">> ->
            case Name of
                <<"presence">> ->
                    %% ...
                <<"message">> ->
                    %% ...
                <<"iq">> -> process_iq(From, To, Packet);
                _ -> ok
            end;
        _ ->
            Mod = get_sm_backend(LServer),
            case online(Mod:get_sessions(LUser, LServer, LResource)) of
                [] ->
                    %% ...
                Ss ->
                    %% 获取session 信息后，将 packet 发送给 c2s 进程
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
            end
    end.
```
### c2s handle_info 回调
在c2s module handle_info 回调函数中，会接受`ejabberd_sm:route/3` 发送的packet，并将packet 发送给socket module，最终通过TCP 发送给客户端。

### stanza 流转图
（需要补个图）

# 总结
- 1、 stanza 数据包类型的分类
- 2、 c2s 进程状态转换
- 3、 数据包流转