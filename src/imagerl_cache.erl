-module(imagerl_cache).

%%
%% This module is an Erlang Special Process.
%% See http://www.erlang.org/doc/design_principles/spec_proc.html#id72756
%% This was done so that the process can be used in supervisory tree.
%%
%% OTP exports
-export([start_link/1]).
-export([init/2]).
-export([system_continue/3, system_terminate/4,
         write_debug/3]).
%% My exports
-export([insert/3, lookup/2, delete/2, stats/1]).

-record(cache_entry, 
{
    key        :: binary(), 
    data       :: binary(),
    created_at :: erlang:timestamp()
}).

%%
%% OTP functionality
%%

start_link(CacheName) ->
    proc_lib:start_link(?MODULE, init, [self(), CacheName]).

init(Parent, CacheName) ->
    register(CacheName, self()),
    Deb = sys:debug_options([]),
    true = new(CacheName),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, Deb).

loop(Parent, Deb) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, unused)
    end.

system_continue(Parent, Deb, unused) ->
    loop(Parent, Deb).

system_terminate(Reason, _Parent, _Deb, unused) ->
    exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

%%
%% Custom functionality
%%

%%
%% @doc Create a new image cache
%%
-spec new(CacheName::atom()) -> boolean().

new(CacheName) ->
    CacheName == ets:new(CacheName, [set, public, named_table, {keypos, #cache_entry.key}]).

%%
%% @doc Insert the data associated with the key in the specified cache
%%
-spec insert(CacheName::atom(), Key::binary(), Data::binary()) -> true.

insert(CacheName, Key, Data) ->
    Name = atom_to_list(CacheName),
    Entry = #cache_entry{
        key=Key, 
        data=Data, 
        created_at=os:timestamp()
    },
    estatsd:increment(Name++".insert"),
    true = ets:insert(CacheName, Entry).

%%
%% @doc Find the data associated with the key in the specified cache
%%
-spec lookup(CacheName::atom(), Key::binary()) -> undefined | binary().

lookup(CacheName, Key) ->
    Name = atom_to_list(CacheName),
    case ets:lookup(CacheName, Key) of
        [] -> 
            % Update miss counter here via async call to stats collector
            estatsd:increment(Name++".miss"),
            undefined;
        [#cache_entry{data=Data, created_at=CreatedAt}] -> 
            %  Update hit counter here via async call to stats collector
            estatsd:increment(Name++".hit"),
            %error_logger:info_msg("~p hit ~s (~1024p)~n", [CacheName, keygen:to_hex(Key), CreatedAt]),
            Data
    end.

%%
%% @doc Delete the entry associated with the key from the specified cache
%%
-spec delete(CacheName::atom(), Key::binary()) -> true.

delete(CacheName, Key) ->
    true = ets:delete(CacheName, Key).

%%
%% @doc Get cache-related stats
%%
-spec stats(CacheName::atom()) -> list({Key::atom(), Value::any()}).

stats(CacheName) ->
    WordSize = erlang:system_info(wordsize),
    Info = ets:info(CacheName),
    Filter = fun({K,_}) ->
        case K of
            compressed -> true;
            memory -> true;
            size -> true;
            _ -> false
        end
    end,
    FilteredInfo = lists:filter(Filter, Info),
    Mapper = fun({K,V}) ->
        case K of
            memory -> {K, V * WordSize}; % Convert memory value from words to bytes.
            _ -> {K, V}
        end
    end,
    TransformedInfo = lists:map(Mapper, FilteredInfo),
    % @todo Amend with hit/miss stats
    TransformedInfo.

