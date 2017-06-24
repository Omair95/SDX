-module(client).
-export([test/2]).
-define(timeout, 2000).

test(Host, Resolver) ->

    io:format("Client: looking up ~w~n", [Host]),
    
    Resolver ! {request, self(), Host},
    
    receive
        {reply, {host, Pid}} -> 
                    io:format("Client: sending ping to host ~w ... ", [Host]),
                    Pid ! {ping, self()},
                    receive
                        pong -> io:format("Client: pong reply~n")
                    after ?timeout -> io:format("Client: no reply from host~n")
                    end;
        {reply, unknown} ->
                    io:format("Client: unknown host~n", []), ok;
        Strange -> 
                    io:format("Client: strange reply from resolver: ~w~n", [Strange]),
                    ok
    after ?timeout ->
            io:format("Client: no reply from resolver~n", []),
            ok
end.
