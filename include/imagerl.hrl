
-define(RENDERED_IMAGE_CACHE, rendered_image_cache).
-define(SOURCE_IMAGE_CACHE, source_image_cache).

-type proplist(A, B) :: list({A, B}).

-record(renderReq, {
    url :: string(),
    width = 0 :: non_neg_integer() | 'wurfl',
    height = 0 :: non_neg_integer() | 'wurfl',
    noCache = false :: boolean(),
    format :: string(),
    debug = false :: boolean(),
    annotation :: string(),
    userAgent :: string()
}).
