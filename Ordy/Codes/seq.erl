-module(seq).
-export([null/0, new/1, increment/1, maxfirst/2, max/2, lessthan/2]).

%%% Functions to handle the sequence numbers.
null() ->
    {0,0}.
    
new(Id) ->
    {0, Id}.
    
increment({Pn, Pi}) ->
    {Pn+1, Pi}.
    
maxfirst({Pn, Pi}, {Nn, _}) ->
    {erlang:max(Pn, Nn), Pi}.

max(Proposal, Sofar) ->
    case lessthan(Proposal, Sofar) of
        true ->
            Sofar;
        false ->
            Proposal
    end.
    
lessthan({Pn, Pi}, {Nn, Ni}) ->
    (Pn < Nn) or ((Pn == Nn) and (Pi < Ni)).
