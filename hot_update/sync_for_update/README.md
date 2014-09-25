# how hot update by sync

## perpare


create rebar project 
	
	done, the project  `redink_project`

## compile

	$ ./rebar get-deps; ./rebar com; ./rebar generate; make update
	
## start
	$ ./rel/redink_project/bin/redink_project start
	
## into console
	$ ./rel/redink_project/bin/redink_project attach
	
## test
	1> Pid = erlang:where(redink_project_test).
	
	2> gen_server:call(Pid, {test}).
	
## notice 

**need start sync** via

	application:start(sync).

**need add configure information in sys.config**

	 {sync,[
        {growl, all},
        {log, all},
        {non_descendants, allow},
        {excluded_modules, []},
        {executable, growlnotify}
    ]}
    
## change code

now, we just try to change `gen_server` logic code

just like:

	handle_call({test}, _, State) ->
    	Reply = update1,
    	{reply, Reply, State, ?HIBERNATE_TIMEOUT};
    	
change it's logic code:

	handle_call({test}, _, State) ->
    	Reply = update2,
    	{reply, Reply, State, ?HIBERNATE_TIMEOUT};

that's all , sync will help you update code now.

## warning 

In product system, `sync` will cause some discomfort. You should use it carefully in product system. But, during development, `sync` is so nice to help you update code without any break down.

## final 

any q, @redink .