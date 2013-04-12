-module(dummy_tests).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->  
    {timeout, 60, [fun () ->

    %% @todo 
    ok

    end]}.

config_refresh_success_test() ->
    ?assertEqual(ok, false).

teardown_test_() ->
    {timeout, 60, [fun () ->

    %% @todo
    ok

    end]}.
