-module(gms3).
-export([start/1, start/2]).
-define(timeout, 1000).
-define(arghh, 100).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    leader(Name, Master, [], 1).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, N, Leader, Slaves} = NewLast ->
            Ref = erlang:monitor(process, Leader),
            Master ! joined,
            slave(Name, Master, Leader, Slaves, Ref, N+1, NewLast)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

leader(Name, Master, Slaves, N) ->
    receive
        {mcast, Msg} ->
            bcast(Name, {msg, N, Msg}, Slaves),
            Master ! {deliver, Msg},
            leader(Name, Master, Slaves, N+1);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),
            bcast(Name, {view, N, self(), NewSlaves}, NewSlaves),
            leader(Name, Master, NewSlaves, N+1);
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.

slave(Name, Master, Leader, Slaves, Ref, N, Last) ->
    receive
        {mcast, Msg} = Fwd ->
            Leader ! Fwd,
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {join, Peer} = Fwd ->
            Leader ! Fwd,
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {msg, N, Msg} = NewLast ->
            Master ! {deliver, Msg},
            slave(Name, Master, Leader, Slaves, Ref, N+1, NewLast);
        {view, N, Leader, NewSlaves} = NewLast ->
            slave(Name, Master, Leader, NewSlaves, Ref, N+1, NewLast);
        {'DOWN', Ref, process, Leader, _Reason} ->
            election(Name, Master, Slaves, N, Last);
        {view, N, NewLeader, NewSlaves} = NewLast ->
            erlang:demonitor(Ref, [flush]),
            NewRef = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, NewSlaves, NewRef, N+1, NewLast);
        {msg, I, _} when I < N ->
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {view, I, Leader, _} when I < N ->
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.

bcast(Name, Msg, Nodes) ->
    lists:foreach(fun(Node) ->
                      Node ! Msg,
                      crash(Name, Msg)
                  end,
                  Nodes).

crash(Name, Msg) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~s CRASHED: msg ~w~n", [Name, Msg]),
            exit(no_luck);
        _ ->
            ok
    end.

election(Name, Master, Slaves, N, Last) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Name, Last, Rest),
            bcast(Name, {view, N, Self, Rest}, Rest),
            leader(Name, Master, Rest, N+1);
        [NewLeader|Rest] ->
            Ref = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, Rest, Ref, N, Last)
    end.
