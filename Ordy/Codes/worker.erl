-module(worker).
-export([start/6]).

start(Name, Main, Module, Id, Sleep, Jitter) ->
    spawn(fun() -> init(Name, Main, Module, Id, Sleep, Jitter) end).

init(Name, Main, Module, Id, Sleep, Jitter) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Cast = apply(Module, start, [Id, self(), Jitter]),
    Main ! {join, self(), Cast},
    receive
        {peers, Peers} ->
            Cast ! {peers, Peers},
            Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
            {ok, Tref} = timer:send_after(Wait, new_topic),
            worker(Name, Id, Cast, 1, 1, Sleep, Tref),
            Cast ! stop
    end.
    
worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, Tref) ->
    receive
        {deliver, {FromName, From, Msg, SenderN, Nre}} ->
            io:format("~s RECV(~4w) ; From: ~s(~4w) ; Subject: ~s~n", 
                      [Name, NextRcv, FromName, SenderN, Msg]),
            if
                From == Id ->
                    worker(Name, Id, Cast, NextSnd, NextRcv+1, Sleep, Tref);
                true ->
                    Op = random:uniform(),
                    if (Op >= 0.8) and (Tref /= null) and (Nre < 3) ->
                          cast_response(Name, Id, Cast, Msg, NextSnd, Nre+1),
                          worker(Name, Id, Cast, NextSnd+1, NextRcv+1, Sleep, Tref);
                       true ->
                          worker(Name, Id, Cast, NextSnd, NextRcv+1, Sleep, Tref)
                    end   
            end;
        new_topic ->
            cast_new_topic(Name, Id, Cast, NextSnd),
            Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
            {ok, NewTref} = timer:send_after(Wait, new_topic),
            worker(Name, Id, Cast, NextSnd+1, NextRcv, Sleep, NewTref);
        pause ->
            timer:cancel(Tref),
            worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, null);
        resume ->
            Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
            {ok, NewTref} = timer:send_after(Wait, new_topic),
            worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, NewTref);
        stop ->
            ok;
        Error ->
            io:format("strange message: ~w~n", [Error]),
            worker(Name, Id, Cast, NextSnd, NextRcv, Sleep, Tref)
    end.

cast_new_topic(Name, Id, Cast, N) ->
    Sbj = lists:sublist(shuffle_list("abcdefghijklmnopqrstuvwxyz"), 8),
    Msg = {Name, Id, Sbj, N, 0},
    io:format("~s SEND(~4w) ; Subject: ~s~n", [Name, N, Sbj]),
    Cast ! {send, Msg}.
 
cast_response(Name, Id, Cast, Text, N, Nre) ->
    Sbj = "Re:" ++ Text,
    Msg = {Name, Id, Sbj, N, Nre},
    io:format("~s SEND(~4w) ; Subject: ~s~n", [Name, N, Sbj]),
    Cast ! {send, Msg}.

shuffle_list(L) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].
