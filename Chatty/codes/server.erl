-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),
    register(myserver, ServerPid).

process_requests(Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [From|Clients], 
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients);  
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(From, Clients),  
            broadcast(NewClients, {leave,Name}),
            From ! exit,
            process_requests(NewClients);
        {send, Name, Text} ->
            broadcast(Clients, {message,Name,Text}),
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

%% Local Functions 
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
