# how hot update by rebar 

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
**don't leave the console**
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
    	
## update

$ ./support/upgrade.sh -n redink_project -v 1.0.0.1

## verify update
	3> gen_server:call(Pid, {test}).

## final 
don't be shy, have a try. any q, @redink.
	