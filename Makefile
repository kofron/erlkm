all: get-deps compile dialyze test

get-deps:
	@./rebar get-deps

compile: get-deps
	@./rebar compile

dialyze: compile
	@dialyzer -r ebin ebin/*.beam

test: dialyze
	@./rebar eunit
