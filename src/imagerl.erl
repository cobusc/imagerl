%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc imagerl startup code

-module(imagerl).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).


bootstrap() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(mochiweb),
    ensure_started(estatsd),
    ensure_started(webmachine).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    bootstrap(),
    imagerl_sup:start_link().

%% @spec start() -> ok
%% @doc Start the imagerl server.
start() ->
    bootstrap(),
    application:start(imagerl).

%% @spec stop() -> ok
%% @doc Stop the imagerl server.
stop() ->
    Res = application:stop(imagerl),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(ssl),
    application:stop(crypto),
    application:stop(public_key),
    application:stop(inets),
    Res.
