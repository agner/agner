all: compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

dialyze:
	@./rebar dialyze skip_deps=true

build-plt:
	@./rebar build-plt skip_deps=true

compile: deps
	@./rebar compile

install:
	@install ./agner /usr/local/bin
