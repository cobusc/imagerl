#!/usr/bin/env escript
% vim: syntax=erlang

read_stuff(Port) ->
    receive
        {Port, {data, ConvertedImageAsBinary}} -> 
            io:format("Got reply: ~s~n", [ConvertedImageAsBinary]),
            read_stuff(Port);
        {Port, closed} -> 
            io:format("Port was closed.~n", []), 
            exit("Done.");
        {'EXIT',Port,Reason} -> 
            io:format("Port died. Reason: ~1024p~n", [Reason]),
            exit("Died");
        AnythingElse ->
            io:format("Got unexpected '~p'~n", [AnythingElse])
    after 10000 ->
        io:format("Timeout~n", [])
    end.
                                                
main(_Args) ->
    {ok, ImageAsBinary} = file:read_file("testimage.jpg"),
    P = open_port({spawn, "/usr/bin/convert - -thumbnail '200x600' -"}, [stream, binary, use_stdio]),
    %P = open_port({spawn, "/bin/cat -"}, [stream, binary, use_stdio]),
    %true = port_command(P, <<"Hello">>),
    true = port_command(P, ImageAsBinary),
    true = port_command(P, <<23>>),
    %true = port_close(P),
    read_stuff(P). 
