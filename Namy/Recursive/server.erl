-module(server).
-export([start/0, start/2, stop/0]).
-define(timeout, 1000).
-define(default_TTL, 5000).

start() ->
    register(server, spawn(fun()-> init() end)).

start(Domain, Parent) ->
    register(server, spawn(fun()-> init(Domain, Parent) end)).

stop() ->
    server ! stop,
    unregister(server).

init() ->
    io:format("Server: create root domain~n"),
    Empty = cache:new(),
    server([], Empty, 0, root, null).

init(Domain, Parent) ->
    io:format("Server: create domain ~w at ~w~n", [Domain, Parent]),
    Parent ! {register, Domain, {domain, self()}},
    Empty = cache:new(),
    server([], Empty, default_TTL, Domain, Parent).

server(Entries, Cache, TTL, Domain, Parent) ->
    receive
        {request, From, Req}->
            io:format("Server: received request to solve ~w~n", [Req]),
            Name = lists:last(Req),
            Subdomain = lists:sublist(Req, length(Req) - 1),
            case entry:lookup(Name, Entries) of
                unknown ->
                    io:format("Server: resolve ~w: unknown~n", [Name]),
                    From ! {reply, unknown},
                    server(Entries, Cache, TTL, Domain, Parent);
                {host, Pid} ->
                    io:format("Server: resolve ~w: known host~n", [Name]),
                    From ! {reply, [{[Name], {host, Pid}, TTL}]},
                    server(Entries, Cache, TTL, Domain, Parent);
                {domain, Pid} ->
                    io:format("Server: resolve ~w: known domain~n", [Name]),
                    case Subdomain of
                        [] ->
                            From ! {reply, [{[Name], {domain, Pid}, TTL}]},
                            server(Entries, Cache, TTL, Domain, Parent);
                        _ ->
                            {Replies, NewCache} = resolve(Req, [Name], Pid, Cache, []),
                            case Replies of
                                unknown ->
                                    io:format("Server: resolve ~w: done: unknown~n", [Req]),
                                    From ! {reply, unknown},
                                    server(Entries, NewCache, TTL, Domain, Parent);
                                {host, _} ->
                                    io:format("Server: resolve ~w: done: reply ~w~n", [Req, Replies]),
                                    From ! {reply, [{Req, Replies, TTL}]},
                                    server(Entries, NewCache, TTL, Domain, Parent);
                                _ ->
                                    NewerCache = updatecache(Replies, NewCache),
                                    NewReplies = [{[Name], {domain, Pid}, TTL}|Replies],
                                    io:format("Server: resolve ~w: done: replies ~w~n", [Req, NewReplies]),
                                    From ! {reply, NewReplies},
                                    server(Entries, NewerCache, TTL, Domain, Parent)
                            end
                    end
            end;
        {register, Name, Entry} ->
            Updated = entry:add(Name, Entry, Entries),
            server(Updated, Cache, TTL, Domain, Parent);
        {deregister, Name} ->
            Updated = entry:remove(Name, Entries),
            server(Updated, Cache, TTL, Domain, Parent);
        {ttl, Sec} ->
            server(Entries, Cache, Sec, Domain, Parent);
        status ->
            io:format("Server: List of DNS entries: ~w ~nCache content: ~w ~n", [Entries, Cache]),
            server(Entries, Cache, TTL, Domain, Parent);
        stop ->
            io:format("Server: closing down~n", []),
            if Domain /= root -> Parent ! {deregister, Domain},
                 ok;
               true -> ok
            end;
        Error ->
            io:format("Server: reception of strange message ~w~n", [Error]),
            server(Entries, Cache, TTL, Domain, Parent)
    end.

resolve(Curr, Curr, Pid, Cache, Req) ->
    Pid ! {request, self(), Req},
    io:format("Server: sent request to solve ~w to ~w~n", [Req, Pid]),
    receive
        {reply, unknown} ->
            {unknown, Cache};
        {reply, Replies} ->
            NewReplies = lists:map(fun({Name, Entry, TTL}) ->
                    FullName = lists:append(Name, Curr),
                    {FullName, Entry, TTL}
                end, Replies),
            {NewReplies, Cache}
    after ?timeout ->
        {unknown, Cache}
    end;
resolve(Subdomain, Curr, Pid, Cache, Req) ->
    case cache:lookup(Subdomain, Cache) of
        unknown ->
            io:format("Server: resolve ~w: unknown in cache~n", [Subdomain]),
            [Head|Domain] = Subdomain,
            resolve(Domain, Curr, Pid, Cache, lists:append(Req, [Head]));
        invalid ->
            io:format("Server: resolve ~w: invalid in cache~n", [Subdomain]),
            NewCache = cache:remove(Subdomain, Cache),
            [Head|Domain] = Subdomain,
            resolve(Domain, Curr, Pid, NewCache, lists:append(Req, [Head]));
        {domain, Srv} ->
            io:format("Server: resolve ~w: domain found in cache~n", [Subdomain]),
            Srv ! {request, self(), Req},
            io:format("Server: sent request to solve ~w to ~w~n", [Req, Srv]),
            receive
                {reply, unknown} ->
                    {unknown, Cache};
                {reply, Replies} ->
                    NewReplies = lists:map(fun({Name, Entry, TTL}) ->
                        FullName = lists:append(Name, Subdomain),
                        {FullName, Entry, TTL}
                        end, Replies),
                    {NewReplies, Cache}
            after ?timeout ->
                    {unknown, Cache}
            end;
        {host, Hst} ->
            io:format("Server: resolve ~w: host found in cache~n", [Subdomain]),
            {{host, Hst}, Cache}
	end.

updatecache([], Cache) ->
    Cache;
updatecache([{Name,Entry,TTL}|Replies], Cache) ->
    Expire = time:add(time:now(), TTL),
    NewCache = cache:add(Name, Expire, Entry, Cache),
    updatecache(Replies, NewCache).
