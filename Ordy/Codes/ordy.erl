-module(ordy).
-export([start/4, stop/0, pause/0, resume/0]).

% We use the name of the multicast module (i.e. total) as a parameter to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent. Jitter stands for up to how many milliseconds the messages are delayed in the network. Duration stands for the duration of the experiment in milliseconds

start(Module, Sleep, Jitter, Duration) ->
    register(ordy, spawn(fun() -> init(Module, Sleep, Jitter, Duration) end)).

stop() ->
    ordy ! stop.

pause() ->
    ordy ! pause.

resume() ->
    ordy ! resume.

init(Module, Sleep, Jitter, Duration) ->
    Ctrl = self(),
    spawn('p1@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P1", Ctrl, Module, 1, Sleep, Jitter) 
                          end),
    spawn('p2@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P2", Ctrl, Module, 2, Sleep, Jitter) 
                          end),
    spawn('p3@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P3", Ctrl, Module, 3, Sleep, Jitter) 
                          end),
    spawn('p4@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P4", Ctrl, Module, 4, Sleep, Jitter) 
                          end),
    Workers = collect(4, [], []),
    run(Workers, Duration).

collect(N, Workers, Peers) ->
    if
        N == 0 ->
            lists:foreach(fun(W) -> W ! {peers, Peers} end, Workers),
            Workers;
        true ->
            receive
                {join, W, P} ->
                    collect(N-1, [W|Workers], [P|Peers])
            end
    end.

run(Workers, Duration) ->
    receive
        stop ->
            lists:foreach(fun(W) -> W ! stop end, Workers);
        pause ->
            lists:foreach(fun(W) -> W ! pause end, Workers),
            run(Workers, Duration);
        resume ->
            lists:foreach(fun(W) -> W ! resume end, Workers),
            run(Workers, Duration)
    after Duration ->
        io:format("Stopping...~n"),
        lists:foreach(fun(W) -> W ! stop end, Workers),
        io:format("Stopped~n")
    end.
