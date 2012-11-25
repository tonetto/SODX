-module(opty_client).
-export([start/4]).

-ifdef(debug_opty).
-define(DBG(X,Y,Z), io:format("[OPTY_CLIENT_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(Clients, Updates, Server, ServerOpty) ->
    ?DBG(clients,"Starting the clients", ok),
    ServerOpty ! {clientOpty, self()},
    receive
        {entry, Entries} ->
            ?DBG(clients,"Got the entries from the server", Entries),
            L = startClients(Clients, [], Entries, Updates, Server),
            io:format("Starting: ~w CLIENTS, ~w ENTRIES, " ++
                          "~w UPDATES PER TRANSACTION ~n",
                      [Clients, Entries, Updates]),
            waitStop(L)
    end.

waitStop(L) ->
    receive
        {stop, server} ->
            ?DBG(clients,"Got a stop from the server!", stop),
            stopClients(L)
    end.

startClients(0,L,_,_,_) -> L;

startClients(Clients, L, Entries, Updates, Server) ->
    Pid = client:start(Clients, Entries, Updates, Server),
    startClients(Clients-1, [Pid|L], Entries, Updates, Server).

stopClients([]) -> ok;

stopClients([Pid|L]) ->
    ?DBG(clients,"Stopping client:",Pid),
    Pid ! {stop, self()},
    receive
        {done, Pid} ->
            ?DBG(clients,"Received done from",Pid),
            ok
    end,
    stopClients(L).
