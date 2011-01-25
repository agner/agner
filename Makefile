all: compile

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile
