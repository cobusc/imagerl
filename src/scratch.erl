%% @doc Code scrathcpad. Should ultimately be removed.
%%
-module(scratch).
-export([wurfl_lookup/2]).


wurfl_lookup(UserAgent, WurflPath) ->
    Url = io_lib:format("~s?ua=~s&search=max_image_width|max_image_height&format=json",
                        [WurflPath, mochiweb_util:quote_plus(UserAgent)]),

    {ok, JsonResponse} = httpc:request(lists:flatten(Url)),

    {ok, mochijson2:decode(JsonResponse)}.
    




