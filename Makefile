all: compile

compile:
	@rebar compile

test: force
	rebar eunit skip_deps=true

force: 
	@true
