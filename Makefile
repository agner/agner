all: compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

install:
	@install ./agner /usr/local/bin
