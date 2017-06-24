-module(host).
-export([start/3, stop/1]).

start(Name, Domain, Parent) ->
    register(Name, spawn(fun()-> init(Domain, Parent) end)).

stop(Name) ->
    Name ! stop,
    unregister(Name).

init(Domain, Parent) ->
    io:format("Host: create domain ~w at ~w~n", [Domain, Parent]),
    Parent ! {register, Domain, {host, self()}},
    host(Domain, Parent).

host(Domain, Parent) ->
    receive
        {ping, From} ->
            io:format("Host: Ping from ~w~n", [From]),
            From ! pong,
            host(Domain, Parent);
        stop ->
            io:format("Host: Closing down~n", []),
            Parent ! {deregister, Domain},
            ok;
        Error ->
            io:format("Host: reception of strange message ~w~n", [Error]),
            host(Domain, Parent)
    end.
