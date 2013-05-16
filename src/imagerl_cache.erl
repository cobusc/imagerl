-module(imagerl_cache).
-export([new/1, insert/3, lookup/2, delete/2, stats/1]).

-record(cache_entry, 
    {
        key        :: binary(), 
        data       :: binary(),
        created_at :: erlang:timestamp()
    }).

%%
%% @doc Create a new image cache
%%
-spec new(CacheName::atom()) -> boolean().

new(CacheName) ->
    % @todo Start associated stats collector
    CacheName == ets:new(CacheName, [set, public, named_table, {keypos, #cache_entry.key}]).

%%
%% @doc Insert the data associated with the key in the specified cache
%%
-spec insert(CacheName::atom(), Key::binary(), Data::binary()) -> true.

insert(CacheName, Key, Data) ->
    Entry = #cache_entry{
        key=Key, 
        data=Data, 
        created_at=os:timestamp()
    },
    true = ets:insert(CacheName, Entry).

%%
%% @doc Find the data associated with the key in the specified cache
%%
-spec lookup(CacheName::atom(), Key::binary()) -> undefined | binary().

lookup(CacheName, Key) ->
    case ets:lookup(CacheName, Key) of
        [] -> 
            % @todo Update miss counter here via async call to stats collector
            %error_logger:info_msg("~p miss ~s~n", [CacheName, keygen:to_hex(Key)]),
            undefined;
        [#cache_entry{data=Data, created_at=CreatedAt}] -> 
            % @todo Update hit counter here via async call to stats collector
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
    % @todo Amend with hit/miss stats
    % @todo Convert memory from words to bytes by multiplying with erlang:system_info(wordsize)
    FilteredInfo.

