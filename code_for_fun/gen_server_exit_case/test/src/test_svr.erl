%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) , redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink
%%%-------------------------------------------------------------------
-module(test_svr).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(HIBERNATE_TIMEOUT, 10000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start_link(Tag) ->
    gen_server:start_link({local, sub_process}, ?MODULE, [Tag], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}, ?HIBERNATE_TIMEOUT};
init([nouse]) ->
    erlang:send(erlang:self(), send_self_msg),
    {ok, #state{}, ?HIBERNATE_TIMEOUT};
init([nothing]) ->
    {ok, #state{}, ?HIBERNATE_TIMEOUT};
init([{nothing, trap_exit}]) ->
    _ = erlang:process_flag(trap_exit, true),
    {ok, #state{}, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------

%% test for caller timeout
%% gen_server process is normal
handle_call(test_caller_timeout, _From, State) ->
    timer:sleep(5100),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% test for spawn one process and the process exit unnormal
%% gen_server process will exit too
handle_call(test_spawn_link_process_exit, _From, State) ->
    F = fun() ->
            1 = 2
        end,
    erlang:spawn_link(F),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% gen_server process is normal
handle_call(test_spawn_process_exit, _From, State) ->
    F = fun() ->
            1 = 2
        end,
    erlang:spawn(F),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% gen_server process is normal
handle_call({trap_exit, test_spawn_link_process_exit}, _From, State) ->
    _ = erlang:process_flag(trap_exit, true),
    F = fun() ->
            1 = 2
        end,
    erlang:spawn_link(F),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% gen_server process is normal
handle_call({trap_exit, test_spawn_process_exit}, _From, State) ->
    _ = erlang:process_flag(trap_exit, true),
    F = fun() ->
            1 = 2
        end, 
    erlang:spawn(F),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% gen_server process will exit too
handle_call(test_spawn_genserver_process_exit, _From, State) ->
    %% ensure trap_exit is false
    _ = erlang:process_flag(trap_exit, false),
    ?MODULE:start_link(nouse),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% gen_server process is normal
handle_call({trap_exit, test_spawn_genserver_process_exit}, _From, State) ->
    _ = erlang:process_flag(trap_exit, true),
    ?MODULE:start_link(nouse),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% sub process will exit too
%% sub process will not execute terminate/2
handle_call(test_spawn_genserver_process_and_parent_exit, _From, State) ->
    R = ?MODULE:start_link(nothing),
    io:format(" sub process is : ~p~n", [R]),
    1 = 2,
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

%% sub process will exit too
%% sub process will execute terminate/2
handle_call({trap_exit, test_spawn_genserver_process_and_parent_exit}, _From, State) ->
    R = ?MODULE:start_link({nothing, trap_exit}),
    io:format(" sub process is : ~p~n", [R]),
    1 = 2,
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

% 如果想要使用exit，要注意这里最后的两行，Reason的两个特殊值：normal和kill。
% 使用exit(Pid,normal)的时候，如果pid 是trap_exit的，那么normal会变成一个{'EXIT',From,normal}的消息发送到pid的messges中。
% 使用exit(Pid, kill) 的时候，pid会直接退出。
% 对于gen_server，要注意这些时候都是不会走进terminate的，其他Reason可以正常走进terminate执行清理工作。

handle_call({normal, test_spawn_genserver_process_and_parent_exit}, _From, State) ->
    R = ?MODULE:start_link(nothing),
    io:format(" sub process is : ~p~n", [R]),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

handle_call({normal, {trap_exit, test_spawn_genserver_process_and_parent_exit}}, _From, State) ->
    R = ?MODULE:start_link({nothing, trap_exit}),
    io:format(" sub process is : ~p~n", [R]),
    {reply, ok, State, ?HIBERNATE_TIMEOUT};

handle_call(_Request, _From, State) ->
    {reply, ok, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------

handle_info(send_self_msg, State) ->
    1 = 2,
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
               [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};

handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    io:format("-------- ~p, ~p~n", [erlang:self(), Reason]),
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

