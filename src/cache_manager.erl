-module(cache_manager).
-behaviour(gen_event).
 
-export([init/1, 
         handle_event/2, 
         handle_call/2, 
         handle_info/2, 
         code_change/3,
         terminate/2]).
 
init([]) ->
    {ok, []}.

handle_event({system_memory_high_watermark, []}, State) ->
    io:format("~s: system_memory_high_watermark reached~n", [?MODULE_STRING]),
    {ok, State};

handle_event({process_memory_high_watermark, Pid}, State) ->
    io:format("~s: Process ~p reached process_memory_high_watermark~n", [?MODULE_STRING, Pid]),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
