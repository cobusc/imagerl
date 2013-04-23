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
    Key = keygen:wurfl_ua_key(Req#renderReq.userAgent),

    % @todo Code below needs refactoring
    {Width, Height} = 
    case imagerl_cache:lookup(?WURFL_CACHE, Key) of
        undefined ->
	    % Try to obtain WURFL values, with fallback strategy if it fails
	    case catch wurfl_lookup(Req#renderReq.userAgent, "http://localhost/") of
		{ok, Result} ->
                    imagerl_cache:insert(?WURFL_CACHE, Key, Result),
		    Result;
		_ ->
		  {0, 0}
	    end;
        CachedValue ->
            CachedValue
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
    case imagerl_cache:lookup(?IMAGE_CACHE, Key) of
        undefined ->
            % @todo Have to build in connect timeout and connection timeout here.
            {ok,{{_,200,"OK"}, _Headers, Body}} = httpc:request(binary_to_list(Req#renderReq.url)),
            imagerl_cache:insert(?IMAGE_CACHE, Key, Body),
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
    case imagerl_cache:lookup(?IMAGE_CACHE, Key) of
        undefined ->
            {ok, ComputedValue} = compute_from_source(RenderReq),
            imagerl_cache:insert(?IMAGE_CACHE, Key, ComputedValue),
            ComputedValue;
        CachedValue ->
            CachedValue
    end,
    {ok, Data}.



