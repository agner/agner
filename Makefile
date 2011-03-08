all: compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

dialyze:
	@./rebar clean
	@./rebar compile debug_info=1
	@./rebar dialyze skip_deps=true
	@./rebar clean

build-plt:
	@./rebar build-plt skip_deps=true

compile: deps
	@./rebar compile
	@cp deps/rebar/ebin/rebar* ebin/
	@./rebar escriptize
	@./scripts/add_rebar_templates

install:
	@install ./agner /usr/local/bin
