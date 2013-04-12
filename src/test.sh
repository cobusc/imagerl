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
    BinaryData = <<"Testing a very very very very very very very long string\n">>,
    Size = erlang:size(BinaryData),
    io:format("Payload = ~B bytes~n", [Size]),
    %PortOpts = [{packet, 4}, binary, use_stdio, exit_status, {env, [{"LD_PRELOAD", "./read_interposer.so"}]}],
    %P = open_port({spawn_executable, "/usr/bin/convert"}, [{args, ["-", "-thumbnail", "200x600", "-"]} | PortOpts ]),
    %P = open_port({spawn, "/usr/bin/convert - -thumbnail 200x600 /tmp/test.foo"}, PortOpts),
    P = open_port({spawn, "/bin/echo  > /tmp/test.foo"}, %"strace /usr/bin/hexdump -C > /tmp/test.foo"}, 
                   [{packet, 4}, 
                    binary, 
                    use_stdio, 
                    exit_status, 
                    {env, [{"LD_PRELOAD", "./read_interposer.so"}]}]),

    true = port_command(P, BinaryData),
    %true = port_command(P, <<Size:32/big, BinaryData/binary>>),
    %true = port_close(P),
    
    read_stuff(P). 
