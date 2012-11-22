-module(opty).
-export([start/4, stop/1]).

start(Clients, Entries, Updates, Time) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION," ++
                  "DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
              stop(L).
stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

startClients(0,L,_,_) -> L;

startClients(Clients, L, Entries, Updates) ->
    Pid = client:start(Clients, Entries, Updates, s),
    startClients(Clients-1, [Pid|L], Entries, Updates).

stopClients([]) -> ok;

dstopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
