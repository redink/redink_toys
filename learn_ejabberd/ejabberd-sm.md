
# `Ejabberd` session manage
`Ejabberd session manange` 是管理`ejabberd` 系统中`session` 角色。
## `ejabberd_sm` module 启动
`ejabberd_sm module` 是在`ejabberd_app` module 中调用启动的，在`ejabberd_app`module 中调用`ejabberd_sm:start()` 函数，`ejabberd_sm:start/0` 的实现：

```
start() ->
    ChildSpec =
        {?MODULE, {?MODULE, start_link, []},
         transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).
```

通过向`ejabberd_sup` supervisor 进程添加 child，来start `ejabberd_sm` gen_server 进程：

```
init([]) ->
    %% 1
    lists:foreach(fun(Mod) -> Mod:init() end, get_sm_backends()),
    %% 2
    ets:new(sm_iqtable, [named_table]),
    %% 3
    lists:foreach(
      fun(Host) ->
          ejabberd_hooks:add(roster_in_subscription, Host,
                 ejabberd_sm, check_in_subscription, 20),
          ejabberd_hooks:add(offline_message_hook, Host,
                 ejabberd_sm, bounce_offline_message, 100),
          ejabberd_hooks:add(remove_user, Host,
                 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    %% 4
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.
```

典型的gen_server behavior start_link callback， 调用init/1 函数，在init/1 函数中会：
- 1 获取session manage 的backend，并回调 backend module 的init/0 函数
- 2 创建sm_iqtable ets table
- 3 注册相关的 hook
- 4 向command module register 相应的command

### backend init
在ejabberd 中，session 的manage 支持多种不同的backend，包括：
- 1 mnesia
- 2 redis
- 3 sql

不同的backend，由不同的module 提供支持。redis backend 的 init 函数主要是进行redis table 的clean。
```
init() ->
    clean_table().
```

mnesia backend 的init 函数，是start 相应的gen_server 函数，并创建需要的mnesia table。
```
init([]) ->
    update_tables(),
    mnesia:create_table(session,
            [{ram_copies, [node()]},
             {attributes, record_info(fields, session)}]),
    mnesia:create_table(session_counter,
            [{ram_copies, [node()]},
             {attributes, record_info(fields, session_counter)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:add_table_copy(session_counter, node(), ram_copies),
    mnesia:subscribe(system),
    {ok, #state{}}.
```

## open session
当单个用户完成所有的XMPP 协议的协商、校验之后，ejabberd 就会为单个用户创建相应的session，也就是调用`open_session` 函数。

在 c2s module 的wait_for_bind 调用 `open_session/1` 函数：
```
open_session(StateData) ->
    %% get user/resource/jid/language/ip
    U    = StateData#state.user,
    IP   = StateData#state.ip,
    %% check acl access
    case acl:access_matches(StateData#state.access, #{usr => jid:split(JID), ip => IP},
                            StateData#state.server) of
        allow ->
            %% change shaper
            change_shaper(StateData, JID),
            {Fs, Ts} = ejabberd_hooks:run_fold(roster_get_subscription_lists, StateData#state.server, {[], []}, [U, StateData#state.server]),
            LJID = jid:tolower(jid:remove_resource(JID)),
            Fs1 = [LJID | Fs],
            Ts1 = [LJID | Ts],
            PrivList = ejabberd_hooks:run_fold(privacy_get_user_list, StateData#state.server, #userlist{}, [U, StateData#state.server]),
            Conn = get_conn_type(StateData),
            Info = [{ip, StateData#state.ip}, {conn, Conn}, {auth_module, StateData#state.auth_module}],
            %% open session via call ejabberd_sm module
            ejabberd_sm:open_session(StateData#state.sid, U, StateData#state.server, R, Info),
            UpdatedStateData = StateData#state{conn = Conn, pres_f = ?SETS:from_list(Fs1), pres_t = ?SETS:from_list(Ts1), privacy_list = PrivList},
            {ok, UpdatedStateData};
        _ ->
            %% acl access check not allow
            ejabberd_hooks:run(forbidden_session_hook, StateData#state.server, [JID]),
            ?INFO_MSG("(~w) Forbidden session for ~s", [StateData#state.socket, jid:to_string(JID)]),
            Txt = <<"Denied by ACL">>,
            %% error return
            {error, ?ERRT_NOT_ALLOWED(Lang, Txt)}
    end.
```

而在 ejabberd_sm module 中，open_session/5 如下定义：
```
%% sid :: {timestamp, user_c2s_pid}
%% user
%% server
%% resource
open_session(SID, User, Server, Resource, Priority, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver, [SID, JID, Info]).

open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).
```

但，open_session 函数只是一个中间调用，实际的session 创建是在，set_session 函数，而set_session 同样是调用 backend module 的`set_session/1` 函数
```
-spec set_session(sid(), binary(), binary(), binary(),
                  prio(), info()) -> ok.
set_session(SID, User, Server, Resource, Priority, Info) ->
    %% 简单的类型转换
    LUser     = jid:nodeprep(User),
    LServer   = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    US        = {LUser, LServer},
    USR       = {LUser, LServer, LResource},
    Mod       = get_sm_backend(LServer),
    Mod:set_session(#session{sid = SID, usr = USR, us = US,
                 priority = Priority, info = Info}).
```

接着，redis backend 中的`set_session/1` 函数实现如下：

```
-spec set_session(#session{}) -> ok.
set_session(Session) ->
    %% 同样是简单的类型转换，转换成redis 所需要的数据类型
    T        = term_to_binary(Session),
    USKey    = us_to_key(Session#session.us),
    SIDKey   = sid_to_key(Session#session.sid),
    ServKey  = server_to_key(element(2, Session#session.us)),
    USSIDKey = us_sid_to_key(Session#session.us, Session#session.sid),
    %% 然后是redis qp 操作
    case ejabberd_redis:qp([["HSET", USKey, SIDKey, T], ["HSET", ServKey, USSIDKey, T]]) of
        [{ok, _}, {ok, _}] ->
            ok;
        Err ->
            ?ERROR_MSG("failed to set session for redis: ~p", [Err])
    end.
```
而mnesia backend 中`set_session/1`的操作，仅仅是调用`mnesia:dirty_write/1`写入到mnesia 中。

## close session
在c2s module中，当resume session 或terminate XMPP用户链接时，即需要close 相对应用户的session，close session 是调用`ejabberd_sm:close_session/4` 函数：
```
-spec close_session(sid(), binary(), binary(), binary()) -> ok.
close_session(SID, User, Server, Resource) ->
    %% 类型转换
    LUser     = jid:nodeprep(User),
    LServer   = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod       = get_sm_backend(LServer),
    Info      =
        %% 调用backend module 的delete_session/4 函数
        case Mod:delete_session(LUser, LServer, LResource, SID) of
           {ok, #session{info = I}} -> I;
           {error, notfound} -> []
       end,
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver, [SID, JID, Info]).
```
在redis backend 中，`delete_session/4`函数实现流程：

- 1 查找对应用户的session
- 2 如果能找到，就删掉session，否则，notfound 返回

```
-spec delete_session(binary(), binary(), binary(), sid()) ->
                {ok, #session{}} | {error, notfound}.
delete_session(LUser, LServer, _LResource, SID) ->
    USKey = us_to_key({LUser, LServer}),
    case ejabberd_redis:q(["HGETALL", USKey]) of
    {ok, Vals} ->
        Ss = decode_session_list(Vals),
        case lists:keyfind(SID, #session.sid, Ss) of
            false ->
                {error, notfound};
            Session ->
                SIDKey = sid_to_key(SID),
                ServKey = server_to_key(element(2, Session#session.us)),
                USSIDKey = us_sid_to_key(Session#session.us, SID),
                ejabberd_redis:qp([["HDEL", USKey, SIDKey], ["HDEL", ServKey, USSIDKey]]),
                {ok, Session}
        end;
    Err ->
        ?ERROR_MSG("failed to delete session from redis: ~p", [Err]),
        {error, notfound}
    end.
```
## get session
`get_session` 函数，是在`ejabberd_sm` module 中的`do_route/3` 调用的。当用户需要发送XMPP 消息时，会经过多次route，最终到 `ejabberd_sm:route/3`API，而API会调用 `ejabberd_sm:do_route/3` 函数。

redis backend 中，`get_session/3` 函数实现：
```
-spec get_sessions(binary(), binary(), binary()) ->
    [#session{}].
get_sessions(LUser, LServer, LResource) ->
    USKey = us_to_key({LUser, LServer}),
    case ejabberd_redis:q(["HGETALL", USKey]) of
        {ok, Vals} when is_list(Vals) ->
            [S || S <- decode_session_list(Vals),
                  element(3, S#session.usr) == LResource];
        Err ->
            ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
            []
    end.
```
# 总结
ejabberd 系统中，session 是非常重要的角色，消息的发送严重依赖于session 的管理。<br>
最初，ejabberd 中session 是存储在mnesia 中，而因为各种各样的原因，mnesia 的存储方式存在瓶颈。随着ejabberd 的迭代升级，现在已经支持多种backend 的存储。<br>
不仅限于ejabberd 内置backend，还可以根据自己的需求，开发不同的backend，只需要实现 `ejabberd_sm` behavior 即可。