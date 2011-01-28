all: compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

dialyze:
	@./rebar dialyze

build-plt:
	@./rebar build-plt

compile: deps
	@./rebar compile

install:
	@install ./agner /usr/local/bin
