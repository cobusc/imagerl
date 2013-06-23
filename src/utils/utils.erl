-module(utils).
-export([maybe_set_user_agent/2,
         get_first_header_value/2,
         maybe_rewrite_wurfl_values/1,
         maybe_set_nocache_headers/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("imagerl.hrl").

-type mochiheaders() :: gb_tree().

%%
%% @doc Try to extract user agent from various headers
%%
-spec maybe_set_user_agent(AllHeaders::mochiheaders(), RenderReq::#renderReq{}) -> #renderReq{}.

maybe_set_user_agent(AllHeaders, RenderReq) ->
    % Only set user agent if it was not specified
    case RenderReq#renderReq.userAgent of
        undefined ->
            UserAgentHeaders = ["X-Device-User-Agent", "User-Agent"],
            RenderReq#renderReq{userAgent=get_first_header_value(UserAgentHeaders, AllHeaders)};
        _ ->
            RenderReq
    end.

%%
%% @doc Returns the value of the first header found in a list of specified headers.
%%
-spec get_first_header_value(LookupHeaders::list(string()), AllHeaders::mochiheaders()) -> undefined | binary().

get_first_header_value([], _AllHeaders) ->
    undefined;
get_first_header_value([H|Rest], AllHeaders) ->
    case mochiweb_headers:lookup(H, AllHeaders) of
        none -> 
            get_first_header_value(Rest, AllHeaders);
        {value, {_, Value}} ->
            list_to_binary(Value)
    end.


%%
%% @doc Fill in the appropriate WURFL values when required.
%%
%% Note: The userAgent field needs to be populated for the function to be useful.
%%
-spec maybe_rewrite_wurfl_values(RenderReq::#renderReq{}) -> #renderReq{}.

maybe_rewrite_wurfl_values(#renderReq{width=wurfl}=R) ->
        renderer:rewrite_wurfl_values(R);
maybe_rewrite_wurfl_values(#renderReq{height=wurfl}=R) ->
        renderer:rewrite_wurfl_values(R); 
maybe_rewrite_wurfl_values(#renderReq{}=R) ->
        R.

%%
%% @doc Set the nocache HTTP headers to be returned by WebMachine, if the specified condition is true.
%%
-spec maybe_set_nocache_headers(Condition::boolean(), RequestData::#wm_reqdata{}) -> #wm_reqdata{}.

maybe_set_nocache_headers(Condition, RequestData) ->
    case Condition of
        true ->
            RD1 = wrq:set_resp_header("Cache-Control", "no-cache, must-revalidate", RequestData),
            RD2 = wrq:set_resp_header("Pragma", "no-cache", RD1),
            wrq:set_resp_header("Expires", "-1", RD2);
        false ->
            RequestData
    end.
