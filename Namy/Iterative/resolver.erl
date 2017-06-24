-module(resolver).
-export([start/1, stop/0]).
-define(timeout, 2000).

start(Root) ->
    register(resolver, spawn(fun()-> init(Root) end)).

stop() ->
    resolver ! stop,
    unregister(resolver).

init(Root) ->
    Empty = cache:new(),
    Inf = time:inf(),
    Cache = cache:add([], Inf, {domain, Root}, Empty),
    resolver(Cache).

resolver(Cache) ->
    receive
        {request, From, Req}->
            io:format("Resolver: request from ~w to solve ~w~n", [From, Req]),
            {Reply, Updated} = resolve(Req, Cache),
            From ! {reply, Reply},
            resolver(Updated);
        status ->
            io:format("Resolver: cache content: ~w~n", [Cache]),
            resolver(Cache);
        stop ->
            io:format("Resolver: closing down~n", []),
            ok;
        Error ->
            io:format("Resolver: reception of strange message ~w~n", [Error]),
            resolver(Cache)
    end.

resolve(Name, Cache)->
    io:format("Resolve ~w: ", [Name]),
    case cache:lookup(Name, Cache) of
        unknown ->
            io:format("unknown ~n", []),
            recursive(Name, Cache);
        invalid ->
            io:format("invalid ~n", []),
            NewCache = cache:remove(Name, Cache),
            recursive(Name, NewCache);
        Reply ->
            io:format("found ~w~n", [Reply]),
            {Reply, Cache}
    end.

recursive([Name|Domain], Cache) ->
    io:format("Recursive ~w: ", [Domain]),
    case resolve(Domain, Cache) of
        {unknown, Updated} ->
            {unknown, Updated};
        {{domain, Srv}, Updated} ->
            Srv ! {request, self(), Name},
            io:format("Resolver: sent request to solve [~w] to ~w: ", [Name, Srv]),
            receive
                {reply, unknown, _} ->
                    io:format("unknown ~n", []),
                    {unknown, Updated};
                {reply, Reply, TTL} ->
                    io:format("reply ~w~n", [Reply]),
                    Expire = time:add(time:now(), TTL),
                    NewCache = cache:add([Name|Domain], Expire, Reply, Updated),
                    {Reply, NewCache}
            after ?timeout ->
                io:format("timeout~n", []),
                {unknown, Updated}
            end
    end.
