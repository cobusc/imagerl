%% @doc Supervisor for the imagerl application.

-module(imagerl_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("imagerl.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, App} = application:get_application(?MODULE),
    {ok, Dispatch} = file:consult(filename:join([priv_dir(App),
                                                 "dispatch.conf"])),
    Port = case os:getenv("WEBMACHINE_PORT") of
            false -> 8000;
            AnyPort -> AnyPort
          end,
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    MemSup = {memsup,
              {memsup, start_link, []},
               permanent, 5000, supervisor, []},

    ImageCache = {image_cache,
                  {imagerl_cache, start_link, [?IMAGE_CACHE]},
                   permanent, 5000, worker, [imagerl_cache]},

    WurflCache = {wurfl_cache,
                  {imagerl_cache, start_link, [?WURFL_CACHE]},
                   permanent, 5000, worker, [imagerl_cache]},

    Processes = [Web, MemSup, ImageCache, WurflCache],

    {ok, Pid} = gen_event:start_link(),
    ok = gen_event:add_handler(Pid, cache_manager, []),

    {ok, { {one_for_one, 10, 10}, Processes} }.

%%
%% @doc return the priv dir
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.
