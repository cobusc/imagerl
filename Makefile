ERL ?= erl
APP := imagerl

.PHONY: deps

all: deps
	- ctags -R src/ deps/
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	- rm tags
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
