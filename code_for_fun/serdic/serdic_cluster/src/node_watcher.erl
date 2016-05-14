%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) , redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink
%%%-------------------------------------------------------------------
-module(node_watcher).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , get_master_node/0
        , get_node_type/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(HIBERNATE_TIMEOUT, hibernate).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
get_master_node() ->
    [Node] = [X || {_, X, Y, _} <- get_all_record(), Y =:= master],
    Node.

get_node_type() ->
    [{_, _, NodeType, _}] = mnesia:dirty_read(node_type, erlang:node()),
    NodeType.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    set_slave(),
    erlang:send(erlang:self(), monitor_other_node),
    {ok, #state{}, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_info(monitor_other_node, State) ->
    case proplists:lookup(mnesia, application:which_applications()) of
        none ->
            ignore;
        _ ->
            check_master()
    end,
    erlang:send_after(500, erlang:self(), monitor_other_node),
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

check_master() ->
    case [{X, Time} || {_, X, Y, Time} <- get_all_record(),
                       Y =:= master] of
        [] ->
            process_master();
        [{Node, _}] ->
            case lists:member(Node, cluster_mnesia:running_nodes()) of
                false ->
                    mnesia:dirty_delete(node_type, Node),
                    check_master();
                _ ->
                    [set_slave(X) || X <- cluster_mnesia:running_nodes(),
                                     X =/= Node],
                    [mnesia:dirty_delete(node_type, X) ||
                     X <- dirty_all_keys(node_type) --
                          cluster_mnesia:running_nodes()],
                    ok
            end,
            ok;
        NodeList0 ->
            [{NodeX, _} | OtherNodeList] = lists:ukeysort(2, NodeList0),
            case erlang:node() =:= NodeX of
                true ->
                    [set_slave(X) || {X, _} <- OtherNodeList];
                false ->
                    ignore
            end
    end.

process_master() ->
    [Node1 | _] = RunningNodes = cluster_mnesia:running_nodes(),
    OtherNodeList = RunningNodes -- [erlang:node()],
    case [mnesia:dirty_read(node_type, X) || X <- OtherNodeList] of
        [] ->
            set_master();
        ResList0 ->
            case [{X, Y, Time} || {_, X, Y, Time} <- ResList0, Y =:= master] of
                [] ->
                    set_master(Node1);
                ResList  ->
                    [{Node, _, _} | _] = lists:ukeysort(ResList, 3),
                    set_master(Node)
            end
    end.

set_master() ->
    set_master(erlang:node()).

set_slave() ->
    set_slave(erlang:node()).

set_master(Node) ->
    case mnesia:dirty_read(node_type, Node) of
        [{node_type, Node, master, _}] ->
            ignore;
        _ ->
            mnesia:dirty_write(node_type, {node_type, Node, master, os:timestamp()})
    end.

set_slave(Node) ->
    case mnesia:dirty_read(node_type, Node) of
        [{node_type, Node, slave, _}] ->
            ignore;
        _ ->
            mnesia:dirty_write(node_type, {node_type, Node, slave, os:timestamp()})
    end.

get_all_record() ->
    lists:append([mnesia:dirty_read(node_type, XX)
                  || XX <- get_all_nodes()]).

get_all_nodes() ->
    lists:usort(dirty_all_keys(node_type) ++ cluster_mnesia:running_nodes()).    

dirty_all_keys(Table) ->
    case catch mnesia:dirty_all_keys(Table) of
        {'EXIT', _} -> [];
        A           -> A
    end.

