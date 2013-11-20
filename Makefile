.DEFAULT_GOAL:=compile
.PHONY: deps compile clean

deps:
	rebar get-deps
	rebar compile
	
compile:
	rebar compile skip_deps=true

clean:
	rebar clean
