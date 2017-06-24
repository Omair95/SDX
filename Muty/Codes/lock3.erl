-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId, 0);
        stop ->
            ok
    end.

open(Nodes, MyId, Clock) ->
    receive
        {take, Master, Ref} ->
            NewClock = Clock + 1,
            Refs = requests(Nodes, MyId, NewClock),
            wait(Nodes, Master, Refs, [], Ref, MyId, NewClock, NewClock);
        {request, From, Ref, _, Timestamp} ->
            From ! {ok, Ref},
            NewClock = synch(Clock,Timestamp),
            open(Nodes, MyId, NewClock);
        stop ->
            ok
    end.

requests(Nodes, MyId, Clock) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId, Clock}, 
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId, _, NextClock) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId, NextClock);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, Clock, NextClock) ->
    receive
        {request, From, Ref, ReqId, Timestamp} ->
            NewClock = synch(NextClock, Timestamp),
            if Timestamp < Clock; Timestamp =:= Clock, ReqId < MyId ->
                From ! {ok, Ref},
                wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, Clock, NewClock);
             true -> wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, Clock, NewClock)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, Clock, NextClock);
        release ->
            ok(Waiting),
            open(Nodes, MyId, NextClock)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId, Clock) ->
    receive
        {request, From, Ref, _, Timestamp} ->
            NewClock = synch(Clock, Timestamp),
            held(Nodes, [{From, Ref}|Waiting], MyId, NewClock);
        release ->
            ok(Waiting),
            open(Nodes, MyId, Clock)
    end.

synch(Clock1, Clock2) ->
    if Clock2 > Clock1 -> Clock2;
    true -> Clock1
    end.
