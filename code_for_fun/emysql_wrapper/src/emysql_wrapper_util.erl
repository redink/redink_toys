-module(emysql_wrapper_util).

%%--------------------------------------------------------------------
%% External exports (should only be used by the 'mysql_conn' module)
%%--------------------------------------------------------------------
-export([linkmysql/8,
         exec/2,
         exec/3,
         fetch_all_fun/3,
         fetch_all_fun/4,
         fetch_all/2,
         fetch_all/3,
         unlinkmysql/1,
         for_loop/2]).

%% ====================================================================
%% External functions
%% ====================================================================

linkmysql(PId,Poolsize,Address,Port,User,Password,Database,Code)->
    try emysql:add_pool(PId, Poolsize,User, Password, Address, Port,Database,Code) of
        ok ->
            ok;
        Other ->
            lager:error("mysql util linkmysql error, reason ~p", [Other]),
            error
    catch
        exit : pool_already_exists ->
            ok;
        X : Y ->
            lager:error("mysql util linkmysql crash, ~p, reason ~p", [X, Y]),
            {stop,error}
    end.
    
unlinkmysql(PId)->
    try emysql:remove_pool(PId) of
        _->
            ok
    catch
        exit : pool_not_found ->
            ok;
        X : Y ->
            lager:critical("mysql util unlinkmysql failed, error type ~p, error reason ~p, pool id ~p~n", 
                           [X, Y, PId]),
            error
    end.

%mysql_util:exec(p1,"update workflow_conf set jobID=43"). 
%Return : ok | error

exec(PId,Sql)->
    BinarySql = list_to_binary(Sql),
    try emysql:execute(PId,BinarySql) of 
        {ok_packet,_,_,_,_,_,_} ->
            ok;
        Any ->
            lager:error("mysql util exec failed, error reason ~p, pool id ~p, sql ~p~n", 
                        [Any, PId, Sql]),
            error
    catch
        X : Y ->
            lager:critical("mysql util exec failed, error type ~p, error reason ~p, pool id ~p, sql ~p~n",
                           [X, Y, PId, Sql]),
            error
    end.

exec(PId,Sql,Timeout)->
    BinarySql = list_to_binary(Sql),
    try emysql:execute(PId,BinarySql,Timeout) of 
        {ok_packet,_,_,_,_,_,_} ->
            ok;
        Any ->
            lager:error("mysql util exec failed, error reason ~p, pool id ~p, sql ~p, timeout ~p~n", 
                        [Any, PId, Sql, Timeout]),
            error
    catch
        X : Y ->
            lager:critical("mysql util exec failed, error type ~p, error reason ~p, pool id ~p, sql ~p, timeout ~p~n",
                           [X, Y, PId, Sql, Timeout]),
            error
    end.

%%--------------------------------------------------------------------
%% Function: fetch_all_fun(PId,mysql,SqlText)->ok | error
%% Description: select multi record from Mysql by a Sql command
%%--------------------------------------------------------------------
fetch_all_fun(PId,Sql,Fun) ->
    BinarySql = list_to_binary(Sql),
    try emysql:execute(PId,BinarySql) of 
        { _, _, _, Records, _ } ->
            case Records of 
                Record when is_list(Record) ->
                    lists:foreach(Fun,Record),
                    ok;
                Msg ->
                    lager:error("mysql util fetch all failed, query result ~p, pool id ~p, sql ~p, fun ~p~n",
                                [Msg, PId, Sql, Fun]),
                    error
            end;
        Any ->
            lager:error("mysql util fetch all failed, unknown error type ~p, pool id ~p, sql ~p, function ~p~n",
                        [Any, PId, Sql, Fun]),
            error
    catch
        X : Y ->
            lager:critical("mysql fetch all failed, error type ~p, error reason ~p, pool id ~p, sql ~p, function ~p~n",
                           [X, Y, PId, Sql, Fun]),
            error
    end.

%%--------------------------------------------------------------------
%% Function: fetch_all_fun(PId,mysql,SqlText)->ok | error
%% Description: select multi record from Mysql by a Sql command
%%--------------------------------------------------------------------
fetch_all_fun(PId,Sql,Fun,Timeout) ->
    BinarySql = list_to_binary(Sql),
    try emysql:execute(PId,BinarySql,Timeout) of 
        { _, _, _, Records, _ } ->
            case Records of 
                Record when is_list(Record) ->
                    lists:foreach(Fun,Record),
                    ok;
                Msg ->
                    lager:error("mysql util fetch all failed, query result ~p, pool id ~p, sql ~p, timeout ~p, fun ~p~n",
                                [Msg, PId, Sql, Timeout, Fun]),
                    error
            end;
        Any ->
            lager:error("mysql util fetch all failed, unknown error type ~p, pool id ~p, sql ~p, timeout ~p, function ~p~n",
                        [Any, PId, Sql, Timeout, Fun]),
            error
    catch
        X : Y ->
            lager:critical("mysql fetch all failed, error type ~p, error reason ~p, pool id ~p, sql ~p, timeout ~p, function ~p~n",
                           [X, Y, PId, Sql, Timeout, Fun]),
            error
    end.


%%--------------------------------------------------------------------
%% Function: fetch_all(PId,mysql)->{ok,Record} | error
%% Description: select multi record from Mysql by a Sql command
%%--------------------------------------------------------------------
fetch_all(PId,Sql) ->
    BinarySql = list_to_binary(Sql),
    try emysql:execute(PId, BinarySql) of 
        { _, _, _, Result, _ } ->
            case Result of 
                Record when is_list(Record) ->
                    {ok,Record};
                _ ->
                    lager:error("mysql util fetch all failed, query result ~p, pool id ~p, sql ~p~n",
                                [Result, PId, Sql]),
                    error
            end;
        Any ->
            lager:error("mysql util fetch all failed, unknown error type ~p, pool id ~p, sql ~p~n",
                        [Any, PId, Sql]),
            error
    catch
        X : Y ->
            lager:critical("mysql fetch all failed, error type ~p, error reason ~p, pool id ~p, sql ~p~n",
                           [X, Y, PId, Sql]),
            error
    end.

%%--------------------------------------------------------------------
%% Function: fetch_all(PId,mysql)->{ok,Record} | error
%% Description: select multi record from Mysql by a Sql command
%%--------------------------------------------------------------------
fetch_all(PId,Sql,Timeout) ->
    BinarySql = list_to_binary(Sql),
    try emysql:execute(PId, BinarySql,Timeout) of 
        { _, _, _, Result, _ } ->
            case Result of 
                Record when is_list(Record) ->
                    {ok,Record};
                _ ->
                    lager:error("mysql util fetch all failed, query result ~p, pool id ~p, sql ~p, timeout ~p~n",
                                [Result, PId, Sql, Timeout]),
                    error
            end;
        Any->
            lager:error("mysql util fetch all failed, unknown error type ~p, pool id ~p, sql ~p, timeout ~p~n",
                        [Any, PId, Sql, Timeout]),
            error
    catch
        X : Y ->
            lager:critical("mysql fetch all failed, error type ~p, error reason ~p, pool id ~p, sql ~p, timeout ~p~n",
                           [X, Y, PId, Sql, Timeout]),
            error
    end.


for_loop(F,Count)->
    case Count /= 0 of 
        true ->
            try F() of  
                _ -> 
                    for_loop(F,Count-1)
            catch
                X : Y ->
                    lager:error("mysql util for loop crash, ~p, reason ~p", [X, Y]),
                    error
            end;
        false ->
            ok
    end.
