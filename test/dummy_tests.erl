-module(dummy_tests).
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->  
    {timeout, 60, [fun () ->

    %% @todo 
    ok

    end]}.

some_test() ->
    ?assertEqual(ok, ok).

teardown_test_() ->
    {timeout, 60, [fun () ->

    %% @todo
    ok

    end]}.
