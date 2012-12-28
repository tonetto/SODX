%% Chatty - Server2 code
-module(server2bis).
%% Exported Functions
-export([start/0, start/1, init_server/0, init_server/1]).

%% API Functions
start() ->
    ServerPid = spawn(server2bis, init_server, []),
    register(myserver, ServerPid).
start(BootServer) ->
    ServerPid = spawn(server2bis, init_server, [BootServer]),
    register(myserver, ServerPid).

init_server() ->
    process_flag(trap_exit, true),
    process_requests([], [self()]).

init_server(BootServer) ->
    BootServer ! {server_join_req, self()},
    process_flag(trap_exit, true),
    process_requests([], []).

process_requests(Clients, Servers) ->
    receive
	%% Messages between client and server
	{client_join_req, Name, From} ->
	    NewClients = [From|Clients],
	    broadcast(Servers,{join, Name}),
	    io:format("[DEBUG] ~s client joined~n", [Name]), %% DEUBG
	    process_requests(NewClients, Servers);
	{client_leave_req, Name, From} ->
	    NewClients = lists:delete(From, Clients),
	    broadcast(Servers, {leave, Name}),
	    io:format("[DEBUG] ~s client leaved~n", [Name]), %% DEBUG
	    process_requests(NewClients, Servers);
	{send, Name, Text} ->
	    broadcast(Servers, {message, Name, Text}),
	    io:format("[DEBUG] {send} received from ~s~n", [Name]), %% DEBUG
	    process_requests(Clients, Servers);

	%% Messages between servers
	{server_join_req, From} ->
	    NewServers = [From|Servers],
	    broadcast(NewServers, {update_servers, NewServers}),
	    io:format("[DEBUG] ~w server joined~n", [From]), %% DEBUG
	    process_requests(Clients, NewServers);
	%% added for bug-fix
	{server_leave_req, From} ->
	    NewServers = lists:delete(From, Servers),
	    io:format("[DEBUG] ~w server leaved~n", [From]), %% DEBUG
	    process_requests(Clients, NewServers);
	{update_servers, NewServers} ->
	    io:format("[SERVER UPDATE] ~w~n", [NewServers]),
	    process_requests(Clients, NewServers);
	{disconnect} ->
	    NewServers = lists:delete(self(), Servers),
	    broadcast(NewServers, {update_servers, NewServers}),
	    io:format("[DEBUG] {disconnect} received~n"), %% DEBUG
	    end_session(Clients);
	%% added for bug-fix
	{'EXIT', _Caller, _Reason} ->
	    broadcast(Servers, {server_leave_req, self()}),
	    io:format("[DEBUG] EXIT~n:"), %% DEBUG
	    io:format("[DEBUG] ~w ~s ~n", [_Caller, _Reason]), %% DEBUG
	    end_session(Clients);

	RelayMessage -> %% Whatever other message is relayed to its clients
	    broadcast(Clients, RelayMessage),
            io:format("[DEBUG] RelayMessage~n"), %% DEBUG
	    process_requests(Clients, Servers)
    end.

%% Local Functions
broadcast(PeerList, Message) ->
    Fun = fun(Peer) ->
		  Peer ! Message end,
    lists:map(Fun, PeerList).

end_session(Clients) ->
    broadcast(Clients, {disconnect}),
    unregister(myserver),
    true.
