%% Chatty - Client code - bis to add {disconnect} message handler.
-module(clientbis).
%% Exported Functions
-export([start/2, init_client/2]).

%% API Functions
start(ServerPid, MyName) ->
    ClientPid = spawn(clientbis, init_client, [ServerPid, MyName]),
    register(pc_pid, self()),
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
	    process_request();
	%% added for bug-fix
	{disconnect} ->
	    io:format("Lost connection with the server. Disconnecting...~n"),
	    exit(whereis(pc_pid), [])
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
