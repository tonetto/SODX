-module(opty_server).
-export([start/2]).

-ifdef(debug_opty).
-define(DBG(X,Y,Z), io:format("[OPTY_SERVER_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(Entries, Time) ->
    ?DBG(server,"Starting the server",ok),
    register(server, self()),
    register(s, server:start(Entries)),
    ClientsOpty = waitClients(na, Entries, Time),
    ?DBG(server,"Content of ClientsOpty",ClientsOpty),
    stop(ClientsOpty).

waitClients(na, Entries, Time) ->
    ?DBG(server,"Waiting for messages from the client Opty.", waiting),
    receive
        {clientOpty, From} ->
            ?DBG(server,"Got the first client opty message", From),
            From ! {entry, Entries},
            waitClients(From, Entries, Time);
        _ ->
            io:format("[WARN] How can I receive anything else now?~n"),
            waitClients(na, Entries, Time)
    end;

waitClients(L, Entries, Time) ->
    receive
        {clientOpty, From} ->
            ?DBG(server,"Got another client opty request", From),
            From ! {entry, Entries},
            waitClients([From|L], Entries, Time);
        _ ->
            io:format("[WARN] How can I receive anything else now?~n"),
            waitClients(L, Entries, Time)
    after
        (Time*1000) ->
            ?DBG(server,"Timeout! Time to stop the execution", (Time*1000)),
            L
    end.

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop,server},
    stopClients(L);
stopClients(L) ->
    L ! {stop,server},
    stopClients([]).
