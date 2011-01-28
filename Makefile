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

install:
	@install ./agner /usr/local/bin
