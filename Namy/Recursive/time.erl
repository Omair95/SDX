-module(time).
-export([now/0, add/2, inf/0, valid/2]).

now() ->
    {H, M, S} = erlang:time(),
    H*3600+M*60+S.

inf() ->
    inf.

add(S, T) ->
    S + T.

valid(inf,_) -> true;
valid(C,T) -> C > T.
    
