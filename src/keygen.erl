-module(keygen).

-export([source_image_key/1,
         rendered_image_key/1]).
-include("imagerl.hrl").


%%
%% @doc Generate a source image key based on its URL
%%

-spec source_image_key(Url::binary()) -> binary().

source_image_key(Url) ->
    crypto:sha256(Url).


%%
%% @doc Generate a render image key based on the rendering parameters
%%

-spec rendered_image_key(RenderReq::#renderReq{}) -> binary().

rendered_image_key(#renderReq{url=U,width=W,height=H,format=F,annotation=A}) ->
%    HashData = <<$u, U/binary, $w, W/integer, $h, H/integer, $f, F/binary, $a, A/binary>>,
    HashData = io_lib:format("u~sw~Bh~Bf~pa~p", [U, W, H, F, A]),
    crypto:sha256(HashData).

