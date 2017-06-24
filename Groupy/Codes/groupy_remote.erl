-module(groupy_remote).
-export([start/3, stop/1]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep, [Leader,Node1,Node2,Node3,Node4]) ->
    
    spawn(Leader, fun() -> register(a, worker:start("P1", Module, Sleep)) end),
    
    spawn(Node1, fun() -> register(b, worker:start("P2", Module, {a,Leader} , Sleep)) end),
    
    spawn(Node2, fun() -> register(c, worker:start("P3", Module, {a,Leader}, Sleep)) end),

    spawn(Node3, fun() -> register(d, worker:start("P4", Module, {a,Leader}, Sleep)) end),
    
    spawn(Node4, fun() -> register(e, worker:start("P5", Module, {a,Leader}, Sleep)) end).                  

stop([Leader,Node1,Node2,Node3,Node4]) ->
     {a,Leader} ! stop,
     {b,Node1} ! stop,
     {c,Node2} ! stop,
     {d,Node3} ! stop,
     {e,Node4} ! stop.

stop(Name, Node) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            {Pid,Node} ! stop
    end.

