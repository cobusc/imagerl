-module(keygen).

-export([source_image_key/1,
         rendered_image_key/1,
         wurfl_ua_key/1,
         to_hex/1]).
-include("imagerl.hrl").


%%
%% @doc Generate an image cache key based on the source URL
%%
-spec source_image_key(Url::binary()) -> binary().

source_image_key(Url)
when is_binary(Url) ->
    crypto:hash(sha256, Url).

%%
%% @doc Generate an image cache key based on the rendering parameters
%%
-spec rendered_image_key(RenderReq::#renderReq{}) -> binary().

rendered_image_key(#renderReq{url=U,width=W,height=H,format=F,annotation=A}) ->
%    HashData = <<$u, U/binary, $w, W/integer, $h, H/integer, $f, F/binary, $a, A/binary>>,
    HashData = io_lib:format("u~sw~Bh~Bf~pa~p", [U, W, H, F, A]),
    crypto:hash(sha256, HashData).

%%
%% @doc Generate a WURFL cache key based on the User Agent string
%%
-spec wurfl_ua_key(UserAgent::binary()) -> binary().

wurfl_ua_key(UserAgent)
when is_binary(UserAgent) ->
    crypto:hash(sha256, UserAgent).

%%
%% @doc Return the hexadecimal representation of the key
%%
-spec to_hex(Key::binary()) -> string().

to_hex(Key) ->
    mochihex:to_hex(Key).
