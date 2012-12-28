%% Chatty - Client code
-module(client).
%% Exported Functions
-export([start/2, init_client/2]).

%% API Functions
start(ServerPid, MyName) ->
    ClientPid = spawn(client, init_client, [ServerPid, MyName]),
    process_commands(ServerPid, MyName, ClientPid).

init_client(ServerPid, MyName) ->
    ServerPid ! {client_join_req, MyName, self()},
    process_request().

%% Local Functions
%% This is the background task logic
process_request() ->
    receive
	{join, Name} ->
	    io:format("[JOIN] ~s joined the chat~n", [Name]),
	    process_request();
	{leave, Name} ->
	    io:format("[LEAVE] ~s leaved the chat~n", [Name]),
	    process_request();
	{message, Name, Text} ->
	    io:format("[~s] ~s", [Name, Text]),
	    process_request()
    end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid) ->
    %% Read from standard input and send to server
    Text = io:get_line("-> "),
    if
	Text == "exit\n" ->
	    ServerPid ! {client_leave_req, MyName, ClientPid};
	true ->
	    ServerPid ! {send, MyName, Text},
	    process_commands(ServerPid, MyName, ClientPid)
    end.
