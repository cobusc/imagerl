-module(ui_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {ok, Application} = application:get_application(),
    HtmlFile = filename:join(code:priv_dir(Application), "www/ui.html"),
    {ok, Data} = file:read_file(HtmlFile),
    {Data, ReqData, State}.

