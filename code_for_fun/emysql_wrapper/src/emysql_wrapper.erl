-module(emysql_wrapper).

-include("emysql_wrapper.hrl").

-export([new_pool/1]).

new_pool(PoolMeta) ->
	PoolName = get_keylist_val(<<"poolname">>, PoolMeta, ?MS_DB_POOL_NAME),
    DBIP     = get_keylist_val(<<"dbip">>, PoolMeta, ?MS_DB_IP),
    DBport   = get_keylist_val(<<"dbport">>, PoolMeta, ?MS_DB_PORT),
                
	supervisor:start_child(emysql_wrapper_pool_sup, 
						   [PoolName, 1, 1,
							DBIP,
							DBport,
							get_keylist_val(<<"dbuser">>    , PoolMeta, ?MS_DB_USER    ),
							get_keylist_val(<<"dbpassword">>, PoolMeta, ?MS_DB_PASSWORD),
							get_keylist_val(<<"dbdatabase">>, PoolMeta, ?MS_DB_DATABASE),
							get_keylist_val(<<"dbchst">>    , PoolMeta, ?MS_DB_CHST    )
						]).

get_keylist_val(Key, KeyList, Default) ->
    case catch lists:keyfind(Key, 1, KeyList) of
        {Key, Value} ->
            Value;
        _ ->
            Default
    end.
