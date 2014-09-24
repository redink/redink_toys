.PHONY: deps
.PHONY: rel

clean:
	./rebar clean
	rm -rf ./apps/*/.rebar
	rm -rf ./apps/*/ebin
	rm -rf ./deps/*/.rebar
	rm -rf ./deps/*/ebin
	rm -rf ./rel/redink_project/

update:
	cp ./support/install_upgrade.escript ./rel/files/
	cp ./support/reltool.config.template ./rel/
