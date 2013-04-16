ERL ?= erl
APP := imagerl

.PHONY: deps

all: deps
	- ctags -R src/ deps/
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	- rm -f tags
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test: all
	@./rebar skip_deps=true eunit

release: all
	@./rebar --force generate
