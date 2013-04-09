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
    %{ok, BinaryData} = file:read_file("testimage.jpg"),
    BinaryData = <<"Testing a very very very very very very very long string">>,
    Size = erlang:size(BinaryData),
    PortOpts = [stream, binary, use_stdio, exit_status, {env, [{"LD_PRELOAD", "./read_interposer.so"}]}],
    %P = open_port({spawn_executable, "/usr/bin/convert"}, [{args, ["-", "-thumbnail", "200x600", "-"]} | PortOpts ]),
    P = open_port({spawn, "/bin/cat -"}, PortOpts),
    %true = port_command(P, <<"Hello">>),
    true = port_command(P, <<Size:32/big>>),
    true = port_command(P, BinaryData),
    %true = port_close(P),
    
    read_stuff(P). 
