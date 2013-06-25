-module(fetch).
-export([url/1
        ]). 


-spec url(Url::string()) -> {ok, binary()} | {error, Reason::any()}.

url(Url) ->
    case catch mochiweb_util:urlsplit(Url) of 
        {"file", _, Path, _, _} -> % "file://<path_to_file>"
            file_handler(Path);
        {"s3", Bucket, [$/ | Key], _, _} -> % "s3://<bucket>/<key>"
            {_Expires, Prefix, QueryString} = erlcloud_s3:make_link(300, Bucket, Key),
            SignedUrl = Prefix ++ QueryString,
            io:format("Signed URL='~s'~n", [SignedUrl]),
            default_handler(SignedUrl);
        {_Scheme, _Netloc, _Path, _Query, _Fragment} -> 
            default_handler(Url);
        _ -> {error, invalid_url}
    end.

%%
%% @doc File system URL handler
%%
-spec file_handler(Path::string()) -> {ok, binary()} | {error, Reason::any()}.

file_handler(Path) ->
    %% @todo There should be a configurable base path (defaulting to priv/www) which should be used as a prefix
    file:read_file(Path).

%%
%% @doc Default URL handler using the httpc library
%%
-spec default_handler(Url::string()) -> {ok, binary()} | {error, Reason::any()}.

default_handler(Url) ->
    Options = [{body_format, binary}],
    Headers = [],
    {ok, HttpGetOptions} = application:get_env(imagerl, http_get_options),
    case httpc:request(get, {Url, Headers}, HttpGetOptions, Options) of
        {ok, {{_,200,"OK"}, _Headers, Body}} -> 
            {ok, Body};
        {ok, Non200Response} ->
            {error, Non200Response};
        {error, _Reason} = Rest ->
            Rest
    end.



