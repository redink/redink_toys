%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) redink
%%% @doc
%%%
%%% @end
%%% Created :  by redink's ptmysql_sum template
%%%-------------------------------------------------------------------
-module({{ptmysql_sum_id}}).

-behaviour(gen_fsm).
-include("mysql_query_conf.hrl").
%% add ?MODULE's include file :)

%% API
-export([querysql/1]).
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% gen_flow callbacks
-export([handle_parse/2, handle_request/2]).
-export([parse/2, request/2]).
%% gen_parse callbacks
-export([handle_from/1, handle_select/1, handle_group/1, 
         handle_order/1, handle_where/2, handle_limit/1]).

-ifdef(HIBERNATE).
-export([wait_for_reqid/1]).
-endif.

-import(ptquery_mysql_interface_util, [get_condition_params/2]).

-define(SERVER, ?MODULE).

-record(state, {taskowner,
                conditionlist,
                sqllist}).

%%--------------------------------------------------------------------
-ifdef(HIBERNATE).

querysql([TaskOwner, OriginReqID, Index, SysList, ConditionList]) ->
    Me    = self(),
    ReqID = mysql_query_common:mk_reqid(Me),
    _Pid  = ?MODULE:start_link({ReqID, Me}, [SysList, ConditionList]),
    proc_lib:hibernate(?MODULE, wait_for_reqid, [[ReqID, TaskOwner, OriginReqID, Index]]).

wait_for_reqid([ReqID, TaskOwner, OriginReqID, Index]) ->
    receive
        {ReqID, {true, Data}} ->
            erlang:send(TaskOwner, {OriginReqID, self(), {Index, {true, Data}}});
        {ReqID, {debug_sql, Data}} ->
            erlang:send(TaskOwner, {OriginReqID, self(), {Index, {debug_sql, Data}}});
        {ReqID, {debug_sql_time, Data}} ->
            erlang:send(TaskOwner, {OriginReqID, self(), {Index, {debug_sql_time, Data}}});
        _Any ->
            lager:error("mysql query ~p result error: ~p~n", [?MODULE, _Any]),
            erlang:send(TaskOwner, {OriginReqID, self(), {Index, {false, []}}})
    after ?MS_QUERY_TIMER ->
            erlang:send(TaskOwner, {OriginReqID, self(), {Index, {false, []}}})
    end.
            
-else.

querysql([SysList, ConditionList]) ->
    gen_flow:handle_querysql(?MODULE, SysList, ConditionList).

-endif.
%%--------------------------------------------------------------------

start_link(TaskOwner, [SysList, ConditionList]) ->
    gen_fsm:start(?MODULE, [TaskOwner, [SysList, ConditionList] ], []).

init([TaskOwner, [SysList, ConditionList] ]) ->
    gen_flow:handle_init(TaskOwner, SysList, ConditionList).

%%--------------------------------------------------------------------

parse(timeout, State) ->
    {next_state,
     request,
     State#state{sqllist = gen_flow:parse(?MODULE, State)}, 0}.

request(timeout, State = #state{taskowner = {ReqID, TaskOwner},
                                conditionlist = [SysList, ConditionList],
                                sqllist = SQLList}) ->
    ?IO_FORMAT(" --@@@@099001- ~p~n", [SQLList]),
    case get_condition_params(<<"debug_sql">>, SysList) of
        <<"true">> ->
            %% debug for sql parse handling
            erlang:send(TaskOwner, {ReqID, {debug_sql, SQLList}});
        _ ->
            case get_condition_params(<<"debug_sql_time">>, SysList) of
                <<"true">> ->
                    %% debug for sql execute time

                    %% get tablename for get table count
                    TableName = "ptmind_"
                        ++ ptquery_mysql_common:term_to_list(
                             get_condition_params(<<"sid">>, ConditionList))
                        ++ "."
                        ++ get_condition_params(<<"tablename">>, ConditionList),
                    erlang:send(TaskOwner, {ReqID, gen_flow:request(?MODULE, SQLList,
                                                                    debug_sql_time, TableName)});
                _ ->
                    %% normal handling
                    erlang:send(TaskOwner, {ReqID, gen_flow:request(?MODULE, SQLList)})
            end
    end,
    {stop, normal, State}.


state_name(_Event, State) ->
    {next_state, state_name, State}.


state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(normal, _StateName, _State) ->
    ok;

terminate(Reason, StateName, #state{taskowner = {_ReqID, TaskOwner}}) ->
    lager:error("module (~p) process is terminated for reason is ~p, and state is ~p", 
                [?MODULE, Reason, StateName]),
    erlang:send(TaskOwner, {exit, false}),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================

handle_parse(_SysList, ConditionList) ->
    case _TableName = get_condition_params(<<"tablename">>, ConditionList) of
        ?FAKE_TABLE ->
            %% retain judge of fake_table
            %% if the tablename is <<"fake_table">>
            [];
        _ ->
            %% when tablename is not <<"fake_table">>
            %% you should parse conditionlist for the final SQL
            %% and the result format should [{GroupByNum, RiNum, RiExpNum, SQL}]
            [{0, 0, 0, "final SQL statements"}]
    end.

handle_request(_, QueryResult) ->
    QueryResult.



%%%======================================================================
%% handle_select function will return `SELECT` of SQL depend Ri or FunName
%% Ri or FunName as `Key` and `SELECT` is the value
%% in SQL statement, `SELECT` is absolutely necessary

handle_select(Ri) ->
    %% the 'something_to_modify' value depend on ?MODULE's include file
    case catch lists:keyfind(Ri, 1, something_to_modify) of
        {Ri, Val} ->
            Val;
        _ ->
            ok %% need change to default value
    end.

%%%======================================================================
%% handle_from function will return `FROM` of SQL depend TableName
%% TableName as `Key` and `FROM` is the value

handle_from(_) ->
    {ok, []}.

%%%======================================================================
%% just want to retain get_pid_filter/2
%% if one module need not this function delete it feel free

handle_where(_, _) ->
    "".

%%%======================================================================
%% for now, the function of handle_order is not necessary
%%
%% because ptnature_mysql not necessary order the result from MySQL server
%% and ptnature_ecolgy will do

handle_order(_) ->
    "".

%%%======================================================================
%% just like handle_order function
%% the function of handle_limit is not necessary too
%%
%% but, somewhere(table interface), this function will need

handle_limit(_) ->
    "".

%%%======================================================================
%% handle_group function is important like handle_select and handle_from.
%% FunName often treated as the `Key`.

handle_group(_) ->
    "".

%%%===================================================================
%%% Internal functions
%%%===================================================================
