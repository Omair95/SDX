-module(basic).
-export([start/3]).

start(Id, Master, Jitter) ->
    spawn(fun() -> init(Id, Master, Jitter) end).

init(Id, Master, Jitter) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    receive
        {peers, Nodes} ->
            server(Id, Master, lists:delete(self(), Nodes), Jitter)
    end.

server(Id, Master, Nodes, Jitter) ->
    receive
        {send, Msg} ->
            multicast(Msg, Nodes, Jitter),
            Master ! {deliver, Msg},
            server(Id, Master, Nodes, Jitter);
        {multicast, Msg} ->
            Master ! {deliver, Msg},
            server(Id, Master, Nodes, Jitter);
        stop ->
            ok
    end.

multicast(Msg, Nodes, 0) ->
    lists:foreach(fun(Node) -> 
                      Node ! {multicast, Msg} 
                  end, 
                  Nodes);
multicast(Msg, Nodes, Jitter) ->
    lists:foreach(fun(Node) -> 
                      T = random:uniform(Jitter),
                      timer:send_after(T, Node, {multicast, Msg})
                  end, 
                  Nodes).
