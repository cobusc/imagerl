%% @doc Code scrathcpad. Should ultimately be removed.
%%
-module(scratch).
-export([wurfl_lookup/2,
         do_everything/1]).
-include("imagerl.hrl").

wurfl_lookup(UserAgent, WurflPath) ->
    Url = io_lib:format("~s?ua=~s&search=max_image_width|max_image_height&format=json",
                        [WurflPath, mochiweb_util:quote_plus(UserAgent)]),

    {ok, JsonResponse} = httpc:request(lists:flatten(Url)),

    {ok, mochijson2:decode(JsonResponse)}.
    

%% @todo Refactor
compute_from_source(Req) ->
    Key = keygen:source_image_key(Req#renderReq.url),
    Data =
    case ets:lookup(?SOURCE_IMAGE_CACHE, Key) of
        [{Key, CachedValue}] -> 
            CachedValue;
        [] ->
            {ok,{{_,200,"OK"}, _Headers, Body}} = httpc:request(binary_to_list(Req#renderReq.url)),
            ets:insert(?SOURCE_IMAGE_CACHE, {Key, Body}),
            Body
    end,

    %% @todo Obviously this is highly inefficient. Will be optimised.    
    InTmpName = test_server:temp_name("/tmp/imagerl.in."),
    OutTmpName = test_server:temp_name("/tmp/imagerl.out."),
    ok = file:write_file(InTmpName, Data),
    Cmd = io_lib:format("convert ~s -thumbnail ~Bx~B! ~s", 
                        [InTmpName, Req#renderReq.width, Req#renderReq.height, OutTmpName]),
    os:cmd(Cmd),
    {ok, Result} = file:read_file(OutTmpName),
    ok = file:delete(InTmpName),
    ok = file:delete(OutTmpName),
    {ok, Result}.


do_everything(RenderReq) ->
    Key = keygen:rendered_image_key(RenderReq),
    Data = 
    case ets:lookup(?RENDERED_IMAGE_CACHE, Key) of
        [{Key, CachedValue}] -> 
            CachedValue;
        [] -> 
            {ok, ComputedValue} = compute_from_source(RenderReq),
            ets:insert(?RENDERED_IMAGE_CACHE, {Key, ComputedValue}),
            ComputedValue
    end,
    {ok, Data}.
