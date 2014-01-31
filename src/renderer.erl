%%
%% @doc The renderer module.
%%
-module(renderer).
-export([wurfl_lookup/1,
         rewrite_wurfl_values/1,
         render/1]).
-include("imagerl.hrl").

wurfl_lookup(UserAgent) ->
    % WURFL Cloud request format
    Url = "http://340259:vj3aXWBUfkzryTgP4mZY0qK5RGNMcDl2@api.wurflcloud.com/v1/json/search:(max_image_width,max_image_height)",

%   WURFL webservice.php query format 
%    Url = io_lib:format("~s?ua=~s&search=max_image_width|max_image_height&format=json",
%                        [WurflPath, mochiweb_util:quote_plus(UserAgent)]),

    StringUrl = lists:flatten(Url),
    Headers = [{"User-Agent", binary_to_list(UserAgent)}],
    {ok, HttpGetOptions} = application:get_env(imagerl, http_get_options),
    StartTime = os:timestamp(),
    try
        {ok,{{_,200,"OK"}, _Headers, JsonResponse}} = httpc:request(get, {StringUrl, Headers}, HttpGetOptions, []),

%    {ok, JsonResponse} = fetch:url(StringUrl),

    % {"apiVersion": "WurflCloud 1.5.0",
    %  "mtime": 1369720128,
    %  "id": "samsung_sgh_e250i_ver1",
    %  "capabilities": {"max_image_width": 120,
    %                   "max_image_height": 130},
    %   "errors": {}
    % }

%        io:format("JSON response: ~p~n", [JsonResponse]),
        {struct, Info} = mochijson2:decode(JsonResponse),
        {<<"capabilities">>, {struct, Capabilities}} = proplists:lookup(<<"capabilities">>, Info),
        {<<"max_image_width">>, W} = proplists:lookup(<<"max_image_width">>, Capabilities),
        {<<"max_image_height">>, H} = proplists:lookup(<<"max_image_height">>, Capabilities),

        {ok, {W,H}}
    catch
        Error ->
            {error, Error}
    after
        estatsd:timing("wurfl.lookup", StartTime)
    end.


rewrite_wurfl_values(Req) ->
    Key = keygen:wurfl_ua_key(Req#renderReq.userAgent),

    % @todo Code below needs refactoring
    {WurflWidth, WurflHeight} = 
    case maybe_use_cache(?WURFL_CACHE, Key, Req#renderReq.noCache) of
        undefined ->
	    % Try to obtain WURFL values, with fallback strategy if it fails
	    case wurfl_lookup(Req#renderReq.userAgent) of
		{ok, Result} ->
                    imagerl_cache:insert(?WURFL_CACHE, Key, Result),
		    Result;
		_ ->
		  {0, 0}
	    end;
        CachedValue ->
            CachedValue
    end,

    {ok, MaxWidth} = application:get_env(imagerl, max_width),
    {ok, MaxHeight} = application:get_env(imagerl, max_height),

    {Width, Height} = {min(WurflWidth,MaxWidth), min(WurflHeight, MaxHeight)},

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
    io_lib:format("-thumbnail ~Bx~B!", [W, H]).


compute_from_source(Req) ->
    StartTime = os:timestamp(),
    {ok, SourceImageCachingEnabled} = application:get_env(imagerl, source_image_caching_enabled),
    StringUrl = binary_to_list(Req#renderReq.url),

    SourceImage =
    case SourceImageCachingEnabled of
        false ->
            {ok, Body} = fetch:url(StringUrl),
            Body;
        true ->
            Key = keygen:source_image_key(Req#renderReq.url),
            case maybe_use_cache(?IMAGE_CACHE, Key, Req#renderReq.noCache) of
                undefined ->
                    {ok, Body} = fetch:url(StringUrl),
                    imagerl_cache:insert(?IMAGE_CACHE, Key, Body),
                    Body;
                CachedValue ->
                    CachedValue
            end
    end,

    % Image processing via stdin/stdout to the /usr/bin/convert command
    Command = lists:flatten(io_lib:format("/usr/bin/convert - ~s -", [get_command_params(Req)])),
    Executable = get_base_dir(?MODULE) ++ "/priv/stdin_forcer",
    P = open_port({spawn_executable, Executable},
                  [stream, use_stdio, stderr_to_stdout, binary, eof,
                  {args, string:tokens(Command, " ")}]),
    L = erlang:size(SourceImage),
    true = port_command(P, <<L:32/integer>>),
    true = port_command(P, SourceImage),
    Response = gather_response(P),
    true = port_close(P),
    estatsd:timing("compute.source", StartTime),
    {ok, Response}.
  

get_base_dir(Module) ->
  {file, Here} = code:is_loaded(Module),
  filename:dirname(filename:dirname(Here)).


gather_response(Port) ->
    gather_response(Port, []).
gather_response(Port, Accum) ->
    receive
        {Port, {data, Bin}} -> gather_response(Port, [Bin | Accum]);
        {Port, eof} -> list_to_binary(lists:reverse(Accum))
    after % max 30 seconds of time for the process to send EOF (close stdout)
        30000 -> died
    end.

%%
%% @doc Render an image according to the request
%%
%% Note: The width and height fields may not be 'wurfl'. It should already have been transformed at this stage.
%%
-spec render(RenderReq::#renderReq{}) -> {ok, Data::binary()}.

render(#renderReq{width=Width, height=Height} = RenderReq)
when is_integer(Width), Width >=0,
     is_integer(Height), Height >= 0 ->
    Key = keygen:rendered_image_key(RenderReq),
    Data =
    case maybe_use_cache(?IMAGE_CACHE, Key, RenderReq#renderReq.noCache) of
        undefined ->
            {ok, ComputedValue} = compute_from_source(RenderReq),
            imagerl_cache:insert(?IMAGE_CACHE, Key, ComputedValue),
            ComputedValue;
        CachedValue ->
            CachedValue
    end,
    {ok, Data}.


-spec maybe_use_cache(CacheName::atom(), Key::binary(), BypassCache::boolean()) -> undefined | binary().

maybe_use_cache(CacheName, Key, BypassCache) ->
    case BypassCache of
        true -> 
            undefined;
        false ->
            imagerl_cache:lookup(CacheName, Key)
    end.

