%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) , redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink
%%%-------------------------------------------------------------------
-module(monmgr_client).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([ register/2
        , unregister/2
        , get_nodelist/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(HIBERNATE_TIMEOUT, 10000).
-define(RECONNECT_INTERVAL, 300*1000).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(GroupName, NodeName) ->
    rpc:call(get_random_node(), monmgr, register,
             [GroupName, NodeName]).

unregister(GroupName, NodeName) ->
    rpc:call(get_random_node(), monmgr, unregister,
             [GroupName, NodeName]).

%% deleteflag :: {delete_old_ymd, YMD}
get_nodelist(TimeZone, YMD, PartKey, DeleteFlag) ->
    rpc:call(get_random_node(), monmgr, get_nodelist,
             [TimeZone, YMD, PartKey, DeleteFlag]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = new_samename_ets(),
    erlang:send(erlang:self(), start_monitor_monmgr),
    {ok, #state{}, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_info(start_monitor_monmgr, State) ->
    MonmgNodeList = get_monmgr_nodelist(),
    ok = monitor_all_nodes(MonmgNodeList),
    [ets:insert(?MODULE, {X, 1}) || X <- MonmgNodeList],
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info({nodedown, DownNode}, State) ->
    case ets:lookup(?MODULE, DownNode) of
        %% did not have this node
        [] ->
            ok;
        [{DownNode, _}] ->
            %% this node down , now reconnect it
            %% first of all , demonitor this node
            %% then , update ets table
            %% erlang:monitor_node(DownNode, false),
            ets:insert(?MODULE, {DownNode, 0}),
            erlang:send(erlang:self(), {reconnect, DownNode}),
            ok
    end,
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info({reconnect, Node}, State) ->
    case net_adm:ping(Node) of
        pong ->
            %% reinsert this node into ets table
            ets:insert(?MODULE, {Node, 1}),
            %% remonitor this node
            erlang:monitor_node(Node, true);
        _A ->
            erlang:send_after(?RECONNECT_INTERVAL, erlang:self(),
                              {reconnect, Node})
    end,
    {noreply, State, ?HIBERNATE_TIMEOUT};


handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
               [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_random_node() ->
    NodeList = [_ | _] = get_avail_node(),
    NodeListLen = erlang:length(NodeList),
    lists:nth(get_random(NodeListLen), NodeList).

get_avail_node() ->
    [Node || {Node, X} <- get_all_node(), X =:= 1].

get_all_node() ->
    ets:tab2list(?MODULE).

new_samename_ets() ->
    ets:new(?MODULE, [set, named_table, public,
                      {write_concurrency, true},
                      {read_concurrency, true}]),
    ok.

get_monmgr_nodelist() ->
    NodeList = application:get_env(monmgr, monmgr_server_list, []),
    parse_type(NodeList).

monitor_all_nodes(MonmgNodeList) ->
    [begin
        pong = net_adm:ping(X),
        true = erlang:monitor_node(X, true)
     end || X <- MonmgNodeList],
    ok.

parse_type(NodeList) ->
    parse_type(NodeList, []).

parse_type([], R) ->
    R;
parse_type([H | T], R) when erlang:is_atom(H) ->
    parse_type(T, [H | R]);
parse_type([H | T], R) when erlang:is_list(H) ->
    parse_type(T, [erlang:list_to_atom(H) | R]).

get_random(Num) ->
    {Res, _} = random:uniform_s(Num, os:timestamp()),
    Res.
