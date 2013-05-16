-module(render_resource).
-export([init/1, 
         to_png/2, 
         content_types_provided/2, 
         allowed_methods/2, 
         malformed_request/2,
         is_authorized/2,
         generate_etag/2,
         forbidden/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("imagerl.hrl").

init(Config) ->
    {ok, Config}.
    %% Enable tracing the decision core for debugging by replacing the line above with this one:
    %%  {{trace, "/tmp"}, Config}.

%%
%% @doc Check if a request is authorised
%%
%% HMAC validation or password checking can be performed here.
%%
-spec is_authorized(ReqData::#wm_reqdata{}, Context::any()) -> 
    {boolean(), NewReqData::#wm_reqdata{}, NewContext::any()}.

is_authorized(ReqData, Context) ->
    {true, ReqData, Context}.

%%
%% @doc Check if the request is forbidden
%%
%% IP checking can be performed here.
%%
-spec forbidden(ReqData::#wm_reqdata{}, Context::any()) -> 
    {boolean(), NewReqData::#wm_reqdata{}, NewContext::any()}.

forbidden(ReqData, Context) ->
    {false, ReqData, Context}.

%%
%% @doc Map content types to their respective handler functions
%%
%-spec content_types_provided(ReqData::#wm_reqdata{}, Context::any()) -> 
%    {list({Mediatype::string(), Handler::fun( #wm_reqdata{},any())}), NewReqData::#wm_reqdata{}, NewContext::any()}.

content_types_provided(ReqData, Context) ->
    %% Map content types to their respective handler functions
    {[ {"image/png", to_png} ], ReqData, Context}.

%%
%% @doc Provide a list of the allowed HTTP methods
%%
-type http_method() :: 'GET' | 'PUT' | 'POST' | 'DELETE' | 'HEAD'.
-spec allowed_methods(ReqData::#wm_reqdata{}, Context::any()) ->
    {list(http_method()), NewReqData::#wm_reqdata{}, NewContext::any()}.
    
allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

%%
%% @doc Check if the request parameters are correct and complete
%%
-spec malformed_request(ReqData::#wm_reqdata{}, Context::any()) ->
    {boolean(), NewReqData::#wm_reqdata{}, NewContext::any()}.

malformed_request(ReqData, Ctx) ->
    QueryArgs = wrq:req_qs(ReqData),
    %% url width heigth nocache format debug annotation ua/HTTP_USER_AGENT

    case create_renderReq(QueryArgs) of
        {ok, RenderReq} ->
            Req = 
            case wrq:get_req_header("User-Agent", ReqData) of
                undefined -> RenderReq;
                UA -> 
                    % io:format("UserAgent implicitly set to '~s'~n", [UA]),
                    RenderReq#renderReq{userAgent=list_to_binary(UA)}
            end,
            {false, ReqData, Req};
        {error, ErrorList} ->
            ReqData2 = wrq:set_resp_body(ErrorList, ReqData),
            {true, ReqData2, Ctx}
    end.

%%
%% @doc Genetate etag
%%
-spec generate_etag(ReqData::#wm_reqdata{}, RenderReq::#renderReq{}) ->
    {Etag::iolist()|undefined, NewReqData::#wm_reqdata{}, RenderReq::#renderReq{}}.

generate_etag(ReqData, RenderReq) ->
    Key = keygen:rendered_image_key(RenderReq),
    Etag = keygen:to_hex(Key),
    {Etag, ReqData, RenderReq}.

%%
%% @doc Response formatting in XXX format.
%%
-spec to_png(ReqData::#wm_reqdata{}, RenderReq::#renderReq{}) -> 
    {Body::iolist(), ReqData::#wm_reqdata{}, Record::#renderReq{}}.

to_png(ReqData, RenderReq) ->
    {ok, Data} = renderer:render(RenderReq),
    %% @todo Cater for different types of images
    {Data, ReqData, RenderReq}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Build a #renderReq record from the specified Args
%%
%% This function fails if there are missing parameters or invalid argument 
%% values specifed.
%%
-spec create_renderReq(Args::proplist(string(), string())) -> {ok, Rec::#renderReq{}} | 
                                                              {error, ErrorList::list(string())}.

create_renderReq(Args) ->
    case populate_renderReq(Args) of 
        {ok, Rec} ->
            % Validate that we have all the arguments
            case is_complete_renderReq(Rec) of
                true  -> {ok, Rec};
                false -> {error, ["Missing parameters. Please refer to the documentation."]}
            end;
        {error, _ErrorList} = E -> E
    end.


%%
%% @doc Check that all record fields have been set.
%%
-spec is_complete_renderReq(Rec::#renderReq{}) -> boolean().

is_complete_renderReq(#renderReq{url=Url,width=Width,height=Height,noCache=NoCache,
                                 format=Format,debug=Debug,annotation=Annotation,
                                 userAgent=UserAgent}) ->
    if 
        is_binary(Url), 
        is_integer(Width) orelse Width =:= wurfl,
        is_integer(Height) orelse Height =:= wurfl,
        is_boolean(NoCache), is_boolean(Debug),
        Format =:= undefined orelse is_binary(Format),
        Annotation =:= undefined orelse is_binary(Annotation),
        UserAgent =:= undefined orelse is_binary(UserAgent) -> true;
        true -> false
    end.


%%
%% @doc Iterative population of a #renderReq{} from a proplist.
%%
-spec populate_renderReq(Args::proplist(string(), string())) -> {ok, Rec::#renderReq{}} | 
                                                                {error, Reason::string()}.

populate_renderReq(Args) ->
    case lists:foldl(fun parse_option/2, {#renderReq{}, []}, Args) of
        {#renderReq{} = Rec, []} -> {ok, Rec};
        {#renderReq{}, ErrorList} -> {error, ErrorList}
    end.

%%
%% @doc Argument parsing
%%
%% The function prototype was chosen so that it will work nicely as an argument to lists:foldl/3.
%%
-type parse_accumulator() :: {Rec::#renderReq{}, ErrorList::list(string())}.
-spec parse_option({Key::string(),Value::string()}, AccIn::parse_accumulator()) -> AccOut::parse_accumulator().

parse_option({"url",V}, {Rec, ErrorList}) ->
    %% @todo Should perform URL format validation here...
    {Rec#renderReq{url = list_to_binary(V)}, ErrorList};

parse_option({"width", V}, {Rec, ErrorList}) ->
    case V of
        "wurfl" -> {Rec#renderReq{width=wurfl}, ErrorList};
        MaybeValue ->
    	    {ok, MaxWidth} = application:get_env(imagerl, max_width),
            case catch list_to_integer(MaybeValue) of 
                {'EXIT', _ } -> {Rec, ["Width should be a non-negative integer or 'wurfl'" | ErrorList]};
                Integer when Integer < 0 ->  {Rec, ["Width may not be negative" | ErrorList]};
                Integer when Integer > MaxWidth -> {Rec, [lists:flatten(io_lib:format("Width may not be larger than ~B", [MaxWidth])) | ErrorList]};
                AllowedInteger -> {Rec#renderReq{width=AllowedInteger}, ErrorList}
            end
    end;

parse_option({"height", V}, {Rec, ErrorList}) ->
    case V of
        "wurfl" -> {Rec#renderReq{height=wurfl}, ErrorList};
        MaybeValue ->
    	    {ok, MaxHeight} = application:get_env(imagerl, max_height),
            case catch list_to_integer(MaybeValue) of 
                {'EXIT', _ } -> {Rec, ["Height should be a non-negative integer or 'wurfl'" | ErrorList]};
                Integer when Integer < 0 -> {Rec, ["Height may not be negative" | ErrorList]};
                Integer when Integer > MaxHeight -> {Rec, [lists:flatten(io_lib:format("Height may not be larger than ~B", [MaxHeight])) | ErrorList]};
                AllowedInteger -> {Rec#renderReq{height=AllowedInteger}, ErrorList}
            end
    end;

parse_option({"nocache", V}, {Rec, ErrorList}) ->
    case V of
        "" -> {Rec#renderReq{noCache=true}, ErrorList};
        _ -> {Rec, ["nocache is a flag and cannot have a value" | ErrorList]}
    end;

parse_option({"debug", V}, {Rec, ErrorList}) ->
    case V of
        "" -> {Rec#renderReq{debug=true}, ErrorList};
        _ -> {Rec, ["debug is a flag and cannot have a value" | ErrorList]}
    end;

parse_option({"format", V},  {Rec, ErrorList}) ->
    {Rec#renderReq{format=list_to_binary(V)}, ErrorList};

parse_option({"annotation", V}, {Rec, ErrorList}) ->
    {Rec#renderReq{annotation=list_to_binary(V)}, ErrorList};

parse_option({"ua", V}, {Rec, ErrorList}) -> 
    {Rec#renderReq{userAgent=list_to_binary(V)}, ErrorList};

parse_option({_Key, _Value}, {_Rec, _ErrorList} = T) ->
    T.
