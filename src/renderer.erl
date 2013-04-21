%% @doc The renderer modile.
%%
-module(renderer).
-export([wurfl_lookup/2,
         render/1]).
-include("imagerl.hrl").

wurfl_lookup(UserAgent, WurflPath) ->
    Url = io_lib:format("~s?ua=~s&search=max_image_width|max_image_height&format=json",
                        [WurflPath, mochiweb_util:quote_plus(UserAgent)]),

    error_logger:info_msg("Calling '~s'", [Url]),
    {ok, JsonResponse} = httpc:request(lists:flatten(Url)),

    % {"apiVersion":"2.1.2",
    %  "useragent":"Mozilla\/5.0",
    %  "id":"mozilla_ver5",
    %  "capabilities":{"max_image_width":600,
    %                  "max_image_height":600},
    %  "errors":[]
    %  }

    Response = mochijson2:decode(JsonResponse),
    {<<"capabilities">>, Capabilities} = proplists:lookup(<<"capabilities">>, Response),
    {<<"max_image_width">>, W} = proplists:lookup(<<"max_image_width">>, Capabilities),
    {<<"max_image_height">>, H} = proplists:lookup(<<"max_image_height">>, Capabilities),

    {ok, {W,H}}.


rewrite_wurfl_values(Req) ->
    % Try to obtain WURFL values, with fallback strategy if it fails
    {Width, Height} =
    case catch wurfl_lookup(Req#renderReq.userAgent, "http://localhost/") of
        {ok, {W,H}} ->
            {W, H};
        _ ->
            {0, 0}
    end,

    WidthSubstitutedReq =
    case Req#renderReq.width of
        wurfl -> Req#renderReq{width=Width};
        _ -> Req
    end,
    HeightSubstitutedReq =
    case WidthSubstitutedReq#renderReq.height of
        wurfl -> WidthSubstitutedReq#renderReq{height=Height};
        _ -> WidthSubstitutedReq
    end,
    HeightSubstitutedReq. 


get_command_params(#renderReq{width=W, height=H}) ->
    get_command_params(W,H).

get_command_params(0,0) ->
    % No resizing
    "";
get_command_params(0,H) ->
    % Aspect ratio intact. Height specified.
    io_lib:format("-thumbnail x~B", [H]);
get_command_params(W,0) ->
    % Aspect ratio intact. Height specified.
    io_lib:format("-thumbnail ~B", [W]);
get_command_params(W,H) ->
    % Aspect ratio not preserved, since width and height are specified.
    io_lib:format("-thumbnail '~Bx~B!'", [W, H]).



%% @todo Refactor
compute_from_source(Req) ->
    Key = keygen:source_image_key(Req#renderReq.url),
    SourceImage =
    case image_cache:lookup(?SOURCE_IMAGE_CACHE, Key) of
        undefined ->
            {ok,{{_,200,"OK"}, _Headers, Body}} = httpc:request(binary_to_list(Req#renderReq.url)),
            image_cache:insert(?SOURCE_IMAGE_CACHE, Key, Body),
            Body;
        CachedValue ->
            CachedValue
    end,

    %% @todo Obviously this is highly inefficient. Will be optimised.    
    InTmpName = test_server:temp_name("/tmp/imagerl.in."),
    OutTmpName = test_server:temp_name("/tmp/imagerl.out."),
    ok = file:write_file(InTmpName, SourceImage),
    Cmd = io_lib:format("convert ~s ~s ~s", 
                        [InTmpName, get_command_params(Req), OutTmpName]),
    os:cmd(Cmd),
    {ok, Result} = file:read_file(OutTmpName),
    ok = file:delete(InTmpName),
    ok = file:delete(OutTmpName),
    {ok, Result}.


render(#renderReq{width=wurfl}=Req) ->
    NewReq = rewrite_wurfl_values(Req),
    render(NewReq);
render(#renderReq{height=wurfl}=Req) ->
    NewReq = rewrite_wurfl_values(Req),
    render(NewReq);
render(RenderReq) ->
    Key = keygen:rendered_image_key(RenderReq),
    Data =
    case image_cache:lookup(?RENDERED_IMAGE_CACHE, Key) of
        undefined ->
            {ok, ComputedValue} = compute_from_source(RenderReq),
            image_cache:insert(?RENDERED_IMAGE_CACHE, Key, ComputedValue),
            ComputedValue;
        CachedValue ->
            CachedValue
    end,
    {ok, Data}.



