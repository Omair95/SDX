-module(worker).
-export([start/3, start/4]).

-define(change, 20).
-define(color, {0,0,0}).

start(Name, Module, Sleep) ->
    spawn(fun() -> init(Name, Module, Sleep) end).

init(Name, Module, Sleep) ->
    Cast = apply(Module, start, [Name]),
    Color = ?color,
    init_cont(Name, Cast, Color, Sleep).

start(Name, Module, Peer, Sleep) ->
    spawn(fun() -> init(Name, Module, Peer, Sleep) end).

init(Name, Module, Peer, Sleep) ->
    Cast = apply(Module, start, [Name, Peer]),
    receive
        joined ->
            Ref = make_ref(),
            Cast ! {mcast, {state_req, Ref}},
            Color = state_transfer(Cast, Ref),
            if Color /= stop ->
                 init_cont(Name, Cast, Color, Sleep),
                 Cast ! stop;
               true ->
                 Cast ! stop
            end;
        {error, Error} ->
            io:format("worker ~s: error: ~s~n", [Name, Error]);
        stop -> 
            ok
    end.

state_transfer(Cast, Ref) ->
    receive
        {deliver, {state_req, Ref}} ->
            receive
                {deliver, {set_state, Ref, Color}} ->
                    Color
            end;
        {join, Peer} ->
            Cast ! {join, Peer},
            state_transfer(Cast, Ref);
        stop -> 
            stop;
        _Ignore ->
            state_transfer(Cast, Ref)
    end.

init_cont(Name, Cast, Color, Sleep) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Gui = gui:start(Name, self()),
    Gui ! {color, Color}, 
    Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
    timer:send_after(Wait, cast_change),
    worker(Name, Cast, Color, Gui, Sleep),
    Gui ! stop.

worker(Name, Cast, Color, Gui, Sleep) ->
    receive
        {deliver, {change_state, N}} ->
            NewColor = change_color(N, Color),
            Gui ! {color, NewColor},
            worker(Name, Cast, NewColor, Gui, Sleep);
        {deliver, {state_req, Ref}} ->
            Cast ! {mcast, {set_state, Ref, Color}},
            worker(Name, Cast, Color, Gui, Sleep);
        {deliver, {set_state, _, _}} ->
            worker(Name, Cast, Color, Gui, Sleep);
        {join, Peer} ->
            Cast ! {join, Peer},
            worker(Name, Cast, Color, Gui, Sleep);
        cast_change ->
            Cast !  {mcast, {change_state, random:uniform(?change)}},
            Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
            timer:send_after(Wait, cast_change),
            worker(Name, Cast, Color, Gui, Sleep);
        stop ->
            Cast ! stop,
            ok;
        Error ->
            io:format("worker ~s: strange message: ~w~n", [Name, Error]),
            worker(Name, Cast, Color, Gui, Sleep)
    end.

change_color(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
