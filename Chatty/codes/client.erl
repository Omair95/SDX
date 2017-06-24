-module(client).
%% Exported Functions
-export([start/2]).

%% API Functions
start(ServerPid, MyName) ->
    ClientPid = spawn(fun() -> init_client(ServerPid, MyName) end),
    process_commands(ServerPid, MyName, ClientPid).

init_client(ServerPid, MyName) ->
    ServerPid ! {client_join_req, MyName, self()},  
    process_requests().

%% Local Functions
%% This is the background task logic
process_requests() ->
    receive
        {join, Name} ->
            io:format("[JOIN] ~s joined the chat~n", [Name]),
            process_requests();
        {leave, Name} ->
            io:format("[LEFT] ~s left the chat ~n", [Name]),
            process_requests();
        {message, Name, Text} ->
            io:format("[~s] ~s", [Name, Text]),
            process_requests();
        serv_down -> io:format("Server is down ~n Bye!"), process_requests();
        exit -> ok
    end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid) ->
    %% Read from standard input and send to server
    Text = io:get_line("Commands -> "),
    if 
        Text  == "exit\n" ->
            ServerPid ! {client_leave_req, MyName, ClientPid}, 
            ok;
        true ->
            ServerPid ! {send, MyName, Text}, 
            process_commands(ServerPid, MyName, ClientPid)
    end.
