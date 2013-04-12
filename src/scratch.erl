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
do_everything(Req) ->
    Url = Req#renderReq.url,
    {ok,{{_,200,"OK"}, _Headers, Body}} = httpc:request(binary_to_list(Url)),
    InTmpName = test_server:temp_name("/tmp/imagerl.in."),
    OutTmpName = test_server:temp_name("/tmp/imagerl.out."),
    ok = file:write_file(InTmpName, Body),
    os:cmd("convert "++InTmpName++" -thumbnail 200x100 "++OutTmpName),
    {ok, Result} = file:read_file(OutTmpName),
    ok = file:delete(InTmpName),
    ok = file:delete(OutTmpName),
    {ok, Result}.


