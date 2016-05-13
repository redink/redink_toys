
-module(cluster_mnesia).

-export([start/0]).
-export([ join_cluster/1
        , leave_cluster/0
        , remove_from_cluster/1
        , cluster_status/0
        , running_nodes/0
        ]).
-export([ del_schema_copy/1
        , ensure_stopped/0
        , delete_schema/0
        , create_table/2
        , copy_table/1
        , copy_table/2
        ]).

-define(IF(Cond, TrueFun, FalseFun),
        (case (Cond) of
            true -> (TrueFun);
            false-> (FalseFun)
        end)).

%% @doc Start mnesia and init mnesia tables.
-spec start() -> ok.
start() ->
    ok = ensure_ok(ensure_data_dir()),
    ok = ensure_ok(init_mnesia_schema()),
    ok = mnesia:start(),
    ok = init_tables(),
    ok = wait_for(tables).

%% @doc Join given node into cluster.
-spec join_cluster(atom()) -> ok.
join_cluster(Node) ->
    ok = ensure_ok(ensure_stopped()),
    ok = ensure_ok(delete_schema()),
    ok = ensure_ok(ensure_started()),
    ok = ensure_ok(connect(Node)),
    ok = ensure_ok(copy_schema(erlang:node())),
    ok = copy_tables(),
    ok = ensure_ok(wait_for(tables)).

%% @doc Leave from cluster.
-spec leave_cluster() -> ok | {error, term()}.
leave_cluster() ->
    case running_nodes() -- [node()] of
        [] ->
            {error, node_not_in_cluster};
        Nodes ->
            case lists:any(fun(Node) ->
                            case leave_cluster(Node) of
                                ok               -> true;
                                {error, _Reason} -> false
                            end
                          end, Nodes) of
                true  -> ok;
                false -> {error, {failed_to_leave, Nodes}}
            end
    end.

%% @doc Remove given node from cluster.
-spec remove_from_cluster(atom()) -> ok | {error, term()}.
remove_from_cluster(Node) when Node =/= node() ->
    case {is_node_in_cluster(Node), is_running_db_node(Node)} of
        {true, true} ->
            ensure_ok(rpc:call(Node, ?MODULE, ensure_stopped, [])),
            ensure_ok(del_schema_copy(Node)),
            ensure_ok(rpc:call(Node, ?MODULE, delete_schema, []));
        {true, false} ->
            ensure_ok(del_schema_copy(Node)),
            ensure_ok(rpc:call(Node, ?MODULE, delete_schema, []));
        {false, _} ->
            {error, node_not_in_cluster}
    end.

%% @doc Print cluster status information.
-spec cluster_status() -> list().
cluster_status() ->
    Running = mnesia:system_info(running_db_nodes),
    Stopped = mnesia:system_info(db_nodes) -- Running,
    ?IF(Stopped =:= [], [{running_nodes, Running}],
            [{running_nodes, Running}, {stopped_nodes, Stopped}]).

%% @doc Create mnesia table.
-spec create_table(atom(), list()) -> ok | {error, any()}.
create_table(Name, TabDef) ->
    ensure_tab(mnesia:create_table(Name, TabDef)).

%% @doc Copy mnesia table.
-spec copy_table(atom()) -> ok.
copy_table(Name) ->
    copy_table(Name, ram_copies).

-spec copy_table(atom(), atom()) -> ok.
copy_table(Name, RamOrDisc) ->
    ensure_tab(mnesia:add_table_copy(Name, node(), RamOrDisc)).

%% @doc Delete schema copy
-spec del_schema_copy(atom()) -> ok | {error, term()}.
del_schema_copy(Node) ->
    case mnesia:del_table_copy(schema, Node) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @private
-spec ensure_data_dir() -> ok | {error, term()}.
ensure_data_dir() ->
    MnesiaDir = mnesia:system_info(directory),
    case filelib:ensure_dir(filename:join(MnesiaDir, foo)) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {mnesia_dir_error, MnesiaDir, Reason}}
    end.

%% @private
-spec init_mnesia_schema() -> ok | {error, term()}.
init_mnesia_schema() ->
    case mnesia:system_info(extra_db_nodes) of
        []    -> mnesia:create_schema([node()]);
        [_|_] -> ok
    end.

%% @private
-spec copy_schema(atom()) -> ok | {error, term()}.
copy_schema(Node) ->
    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, schema, Node, disc_copies}} ->
            ok;
        {aborted, Error} ->
            {error, Error}
    end.

-spec delete_schema() -> ok | {error, term()}.
delete_schema() ->
    mnesia:delete_schema([erlang:node()]).

-spec init_tables() -> ok.
init_tables() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            create_tables();
        [_ | _] ->
            copy_tables()
    end.

-spec create_tables() -> ok.
create_tables() ->
    (get_table_def_module()):mnesia(boot).

-spec copy_tables() -> ok.
copy_tables() ->
    (get_table_def_module()):mnesia(copy).

-spec ensure_stopped() -> ok | {error, term()}.
ensure_stopped() ->
    stopped = mnesia:stop(),
    wait_for(stop).

-spec ensure_started() -> ok | {error, term()}.
ensure_started() ->
    ok = mnesia:start(),
    wait_for(start).

-spec connect(atom()) -> ok | {error, term()}.
connect(Node) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            ok;
        {ok, []} ->
            {error, {failed_to_connect_node, Node}};
        Error ->
            Error
    end.

leave_cluster(Node) when Node =/= erlang:node() ->
    case is_running_db_node(Node) of
        true ->
            ok = ensure_ok(ensure_stopped()),
            ok = ensure_ok(rpc:call(Node, ?MODULE,
                           del_schema_copy, [erlang:node()])),
            ok = ensure_ok(delete_schema());
        false ->
            {error, {node_not_running, Node}}
    end.

is_node_in_cluster(Node) ->
    lists:member(Node, mnesia:system_info(db_nodes)).

is_running_db_node(Node) ->
    lists:member(Node, running_nodes()).

running_nodes() ->
    mnesia:system_info(running_db_nodes).

%% @doc Wait for mnesia to start, stop or tables ready.
-spec(wait_for(start | stop | tables) -> ok | {error, Reason :: atom()}).
wait_for(start) ->
    case mnesia:system_info(is_running) of
        yes      -> ok;
        no       -> {error, mnesia_unexpectedly_stopped};
        stopping -> {error, mnesia_unexpectedly_stopping};
        starting -> timer:sleep(1000), wait_for(start)
    end;
 
wait_for(stop) ->
    case mnesia:system_info(is_running) of
        no       -> ok;
        yes      -> {error, mnesia_unexpectedly_running};
        starting -> {error, mnesia_unexpectedly_starting};
        stopping -> timer:sleep(1000), wait_for(stop)
    end;

wait_for(tables) ->
    Tables = mnesia:system_info(local_tables),
    case mnesia:wait_for_tables(Tables, 600000) of
        ok                   -> ok;
        {error, Reason}      -> {error, Reason};
        {timeout, BadTables} -> {error, {timetout, BadTables}}
    end.

ensure_ok(ok) ->
    ok;
ensure_ok({error, {_Node, {already_exists, _Node}}}) ->
    ok;
ensure_ok({badrpc, _Reason} = Any) ->
    erlang:throw({error, Any});
ensure_ok({error, _Reason} = Any) ->
    erlang:throw(Any).

ensure_tab({atomic, ok}) ->
    ok;
ensure_tab({aborted, {already_exists, _Name}}) ->
    ok;
ensure_tab({aborted, {already_exists, _Name, _Node}}) ->
    ok;
ensure_tab({aborted, Error}) ->
    Error.

get_table_def_module() ->
    application:get_env(serdic_cluster, table_def_mod, cluster_table).
