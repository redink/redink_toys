-define(MS_DB_NODEPOOLNUM, 1).
-define(MS_DB_NODEPOOLCOUNT, 1).

-define(MS_DB_IP, "127.0.0.1").
-define(MS_DB_PORT, 3306).

-define(MS_DB_USER, "xxx").              % mysql用户名
-define(MS_DB_PASSWORD, "xxx").      % mysql密码
-define(MS_DB_DATABASE, "xxx").  % mysql数据库
-define(MS_DB_CHST, utf8).  

-define(MS_DB_POOL_NAME, "mysql_default_pool").
-define(MS_DB_POOL, 
	[mysql_1_pool,  mysql_2_pool,  mysql_3_pool,  mysql_4_pool,  mysql_5_pool,
	 mysql_6_pool]).

-define(SQL_QUERY_TIMEOUT, 150000).
