# using

## compile
	$ ./rebar get-deps; ./rebar com

## start
	
	$ erl -pa ./ebin -pa ./deps/*/ebin
	
	
	1> application:start(emysql_wrapper).
	02:16:34.735 [info] Application lager started on node nonode@nohost
	02:16:34.741 [info] Application crypto started on node nonode@nohost
	02:16:34.746 [info] Application emysql started on node nonode@nohost
	02:16:34.747 [info] Application emysql_wrapper started on node nonode@nohost
	ok
	
## new pool

	PoolMeta = 
				[
				 {<<"poolname">>, string()},
				 {<<"dbip">>, string()},
				 {<<"dbport">>, integer()},
				 {<<"dbuser">>, string()},
				 {<<"dbpassword">>, string()},
				 {<<"dbdatabase">>, string()},
				 {<<"dbchst">>, atom()}
				]
				
	2> PoolMeta = [{<<"poolname">>, "your_pool"}, {<<"dbip">>, "127.0.0.1"}, {<<"dbport">>, 3306}, {<<"dbuser">>, "your"}, {<<"dbpassword">>, "your"}, {<<"dbdatabase">>, "your"}, {<<"dbchst">>, utf8}].
	
	3> emysql_wrapper:new_pool(PoolMeta).
	{ok,<0.79.0>}
	

## query sql
	4> SQL = "show databases".
	
	5> {ok, Pid} = v(3).
	
	6> gen_server:call(Pid, {querysql, SQL, 5000}).
	{ok,[[<<"information_schema">>],
     [<<"mysql">>],
     [<<"performance_schema">>],
     [<<...>>],
     [...]|...]}
     
## new again

	8> PoolMetaOther = [{<<"poolname">>, "your_other_pool"}, {<<"dbip">>, "127.0.0.1"}, {<<"dbport">>, 3306}, {<<"dbuser">>, "your"}, {<<"dbpassword">>, "your"}, {<<"dbdatabase">>, "your"}, {<<"dbchst">>, utf8}].
	
	9> {ok, PidOther} = emysql_wrapper:new_pool(PoolMetaOther).
	{ok,<0.91.0>}

## query again 
	
	10> gen_server:call(PidOther, {querysql, SQL, 5000}).
	{ok,[[<<"information_schema">>],
     [<<"mysql">>],
     [<<"performance_schema">>],
     [<<...>>],
     [...]|...]}

## pool state 

	11> emysql_conn_mgr:pools().
	[{pool,your_other_pool_1,1,"xxx","xxx",
       "127.0.0.1",3306,"xxx",utf8,
       {[{emysql_connection,"#Port<0.3744>",your_other_pool_1,utf8,
                            #Port<0.3744>,<<"5.5.36-log">>,1360392,63487,33,
                            {0,nil},
                            undefined,true,0,1411496783,undefined,false}],
        []},
       {0,nil},
       {[],[]},
       [],0,infinity,false},
 	
	{pool,your_pool_1,1,"xxx","xxx","127.0.0.1",3306,
       "xxx",utf8,
       {[{emysql_connection,"#Port<0.3664>",your_pool_1,utf8,
                            #Port<0.3664>,<<"5.5.36-log">>,1360343,63487,33,
                            {0,nil},
                            undefined,true,0,1411496613,undefined,...}],
        []},
       {0,nil},
       {[],[]},
       [],0,infinity,false}]
     

## final

happy, any q, @redink.