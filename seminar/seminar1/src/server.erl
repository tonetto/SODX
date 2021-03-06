%% Chatty - Server code
-module(server).
%% Exported Functions
-export([start/0, process_request/1]).

%% API Functions
start() ->
    ServerPid = spawn(server, process_request, [[]]),
    register(myserver, ServerPid).

process_request(Clients) ->
    receive
	{client_join_req, Name, From} ->
	    NewClients = [From | Clients],
	    broadcast(NewClients, {join, Name}),
	    process_request(NewClients);
	{client_leave_req, Name, From} ->
	    NewClients = lists:delete(From, Clients),
	    broadcast(Clients, {leave, Name}),
	    process_request(NewClients);
	{send, Name, Text} ->
	    broadcast(Clients, {message, Name, Text}),
	    process_request(Clients)
    end.

%% Local Functions
broadcast(PeerList, Message) ->
    Fun = fun(Peer) ->
		  Peer ! Message end,
    lists:map(Fun, PeerList).
