%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the imagerl application.

-module(imagerl_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for imagerl.
start(_Type, _StartArgs) ->
    imagerl_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for imagerl.
stop(_State) ->
    ok.
